;;; mild.el --- Meld-Inspired Local Diff -*- lexical-binding: t; -*-

;; Keywords: files, tools
;; Version: 0.6.0

;;; Commentary:
;; A simple side-by-side directory comparison tool inspired by Meld.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'transient)

(defgroup mild nil
  "Meld-Inspired Local Diff."
  :group 'tools)

(defcustom mild-filter-globs '(".git" ".DS_Store" "*.elc" "*.pyc")
  "List of glob patterns to filter out of the comparison.
These match recursively."
  :type '(repeat string)
  :group 'mild)

(defcustom mild-filter-groups
  '(("Backups" "#*#" ".#*" "~*" "*~" "*.orig" "*.bak" "*.swp")
    ("OS metadata" ".DS_Store" "._*" ".Spotlight-V100" ".Trashes"
     "Thumbs.db" "Desktop.ini")
    ("Version Control" ".git" ".svn" ".hg" "CVS" "_darcs" ".bzr"
     "_MTN" ".fslckout" "_FOSSIL_" ".fos" ".osc")
    ("Binaries" "*.pyc" "*.a" "*.obj" "*.o" "*.so" "*.la" "*.lib"
     "*.dll" "*.exe")
    ("Media" "*.jpg" "*.gif" "*.png" "*.bmp" "*.wav" "*.mp3"
     "*.ogg" "*.flac" "*.avi" "*.mpg" "*.xcf" "*.xpm"))
  "List of named filter groups.  Each entry is (NAME GLOBS ...).
Globs use the same format as `mild-filter-globs'.
Toggle groups on/off with `mild-toggle-filter-group'."
  :type '(repeat (list (string :tag "Group name")
                       (repeat :tag "Glob patterns" string)))
  :group 'mild)

(put 'mild-filter-globs 'safe-local-variable
     (lambda (val) (and (listp val) (cl-every #'stringp val))))
(put 'mild-filter-groups 'safe-local-variable
     (lambda (val)
       (and (listp val)
            (cl-every (lambda (entry)
                        (and (listp entry) (stringp (car entry))
                             (cl-every #'stringp (cdr entry))))
                      val))))

(defcustom mild-show-filtered nil
  "Whether to show filtered files (matching `mild-filter-globs`)."
  :type 'boolean
  :group 'mild)

(defcustom mild-show-hidden nil
  "Whether to show hidden (dot) files."
  :type 'boolean
  :group 'mild)

(defcustom mild-show-same nil
  "Whether to show identical files."
  :type 'boolean
  :group 'mild)

(defcustom mild-show-different t
  "Whether to show modified files."
  :type 'boolean
  :group 'mild)

(defcustom mild-show-added t
  "Whether to show new (orphan) files."
  :type 'boolean
  :group 'mild)

(defcustom mild-dir-prefix "📁 "
  "Prefix string shown before directory names to distinguish from files.
Set to \"\" for no prefix."
  :type 'string
  :group 'mild)

(defcustom mild-indent-offset 2
  "Indentation offset for each directory level."
  :type 'integer
  :group 'mild)

(defcustom mild-show-tree-lines t
  "Whether to show hierarchical tree lines."
  :type 'boolean
  :group 'mild)

(defcustom mild-simple-comparison t
  "If non-nil, only compare file sizes for diff status.
This is much faster but less accurate than content comparison."
  :type 'boolean
  :group 'mild)

(defun mild--apply-dir-locals ()
  "Apply .dir-locals.el from `default-directory' to the current buffer."
  (hack-dir-local-variables-non-file-buffer))

(cl-defstruct (mild-node
               (:constructor mild-node-create))
  name
  (name-right nil)    ; display name for the right side (defaults to `name')
  left-path
  right-path
  parent
  children
  (expanded t)
  (diff-status nil) ; 'same, 'different, 'left-only, 'right-only, 'filtered
  (is-dir nil)
  (is-filtered nil))

(defvar-local mild--scan-thread nil
  "Current scanning thread for this buffer.")
(defvar-local mild--last-scan-mode 'full
  "Scan mode used for the last refresh: `simple' or `full'.")
(defvar-local mild--active-filter-groups nil
  "List of currently active filter group names.")
(defvar-local mild--right-root nil)
(defvar-local mild--root-node nil)
(defvar-local mild--column-width nil)

(defun mild--glob-to-regexp (glob)
  "Convert GLOB to a regular expression.
If GLOB contains a slash, it matches against the relative path.
Otherwise, it matches against the filename."
  (let ((rx (wildcard-to-regexp glob)))
    (if (string-match-p "/" glob)
        rx
      (concat "^" rx "$"))))

(defun mild--filtered-p (name rel-path)
  "Check if NAME or REL-PATH is filtered by `mild-filter-globs` or hidden."
  (or (and (not mild-show-hidden) (string-prefix-p "." name))
      (cl-some (lambda (glob)
                 (let ((rx (mild--glob-to-regexp glob)))
                   (if (string-match-p "/" glob)
                       (string-match-p rx rel-path)
                     (string-match-p rx name))))
               mild-filter-globs)
      (cl-some (lambda (group)
                 (when (member (car group) mild--active-filter-groups)
                   (cl-some (lambda (glob)
                              (let ((rx (mild--glob-to-regexp glob)))
                                (if (string-match-p "/" glob)
                                    (string-match-p rx rel-path)
                                  (string-match-p rx name))))
                            (cdr group))))
               mild-filter-groups)))

(defun mild-toggle-filter-group (group)
  "Toggle a named filter GROUP on or off.
GROUP is the name of a group defined in `mild-filter-groups'."
  (interactive
   (let ((names (mapcar #'car mild-filter-groups)))
     (unless names (user-error "No filter groups defined (customize `mild-filter-groups')"))
     (list (completing-read "Toggle filter group: " names nil t))))
  (if (member group mild--active-filter-groups)
      (progn
        (setq mild--active-filter-groups (delete group mild--active-filter-groups))
        (message "Filter group \"%s\" disabled" group))
    (push group mild--active-filter-groups)
    (message "Filter group \"%s\" enabled" group))
  (mild-refresh))

(defun mild--get-files (dir)
  "Get all files in DIR except . and .."
  (when (and dir (file-directory-p dir))
    (directory-files dir t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))

(defun mild--compare-files (left right)
  "Compare LEFT and RIGHT files. Return 'same or 'different."
  (if (and left right
           (file-exists-p left)
           (file-exists-p right))
      (let ((is-dir-left (file-directory-p left))
            (is-dir-right (file-directory-p right)))
        (cond ((not (eq is-dir-left is-dir-right)) 'different)
              (is-dir-left 'same) ; Directories are 'same' by default, children will update it
              (t (if (if mild-simple-comparison
                         (eq (file-attribute-size (file-attributes left))
                             (file-attribute-size (file-attributes right)))
                       (= 0 (process-file "diff" nil nil nil "-q"
                                          (expand-file-name left)
                                          (expand-file-name right))))
                     'same
                   'different))))
    (cond (left 'left-only)
          (right 'right-only)
          (t 'same))))

(defun mild--scan-dir (left-dir right-dir &optional parent rel-path)
  "Recursively scan LEFT-DIR and RIGHT-DIR to build the tree.
PARENT is the parent `mild-node`. REL-PATH is the relative path from root."
  (let* ((all-names (delete-dups
                     (append (mapcar #'file-name-nondirectory (mild--get-files left-dir))
                             (mapcar #'file-name-nondirectory (mild--get-files right-dir)))))
         (nodes nil))
    (dolist (name (sort all-names #'string<))
      (thread-yield)
      (let* ((left (and left-dir (expand-file-name name left-dir)))
             (right (and right-dir (expand-file-name name right-dir)))
             (exists-left (and left (file-exists-p left)))
             (exists-right (and right (file-exists-p right)))
             (this-rel-path (if rel-path (concat rel-path "/" name) name))
             (is-dir (or (and exists-left (file-directory-p left))
                         (and exists-right (file-directory-p right))))
             (status (mild--compare-files (if exists-left left nil) (if exists-right right nil)))
             (is-filtered (mild--filtered-p name this-rel-path)))
        
        (when (or (not is-filtered)
                  mild-show-filtered)
          (let ((node (mild-node-create :name name
                                       :left-path (if exists-left left nil)
                                       :right-path (if exists-right right nil)
                                       :parent parent
                                       :is-dir is-dir
                                       :diff-status (if is-filtered 'filtered status)
                                       :is-filtered is-filtered)))
            (when is-dir
              (setf (mild-node-children node)
                    (mild--scan-dir (if (and exists-left is-dir) left nil)
                                    (if (and exists-right is-dir) right nil)
                                    node
                                    this-rel-path))
              ;; Update directory status based on children
              (when (and (not is-filtered) (eq (mild-node-diff-status node) 'same))
                (when (cl-some (lambda (child) (not (eq (mild-node-diff-status child) 'same)))
                               (mild-node-children node))
                  (setf (mild-node-diff-status node) 'different))))
            (push node nodes)))))
    (nreverse nodes)))

(defun mild-diff (dir1 dir2)
  "Compare DIR1 and DIR2 side-by-side."
  (interactive "DLeft directory: \nDRight directory: ")
  (let ((buf (get-buffer-create (format "*mild: %s <-> %s*" 
                                        (file-name-nondirectory (directory-file-name dir1))
                                        (file-name-nondirectory (directory-file-name dir2))))))
    (with-current-buffer buf
      (mild-mode)
      (setq default-directory (file-name-as-directory (expand-file-name dir1)))
      (mild--apply-dir-locals)
      (setq mild--left-root (file-name-as-directory (expand-file-name dir1)))
      (setq mild--right-root (file-name-as-directory (expand-file-name dir2)))
      (setq mild--last-scan-mode (if mild-simple-comparison 'simple 'full))
      (setq mild--root-node (mild-node-create
                              :name (file-name-nondirectory
                                     (directory-file-name mild--left-root))
                              :name-right (file-name-nondirectory
                                           (directory-file-name mild--right-root))
                              :left-path mild--left-root
                              :right-path mild--right-root
                              :is-dir t
                              :expanded t))
      (setf (mild-node-children mild--root-node)
            (mild--scan-dir mild--left-root mild--right-root mild--root-node))
      ;; Set root status based on children
      (when (cl-some (lambda (child) (not (eq (mild-node-diff-status child) 'same)))
                     (mild-node-children mild--root-node))
        (setf (mild-node-diff-status mild--root-node) 'different))
      (mild--render))
    (switch-to-buffer buf)
    (mild-refresh)))

(defun mild--get-dwim-dirs ()
  "Try to guess directories based on visible dired buffers."
  (let ((dired-dirs (delq nil
                          (mapcar (lambda (w)
                                    (with-current-buffer (window-buffer w)
                                      (when (eq major-mode 'dired-mode)
                                        (directory-file-name (expand-file-name default-directory)))))
                                  (window-list)))))
    (delete-dups dired-dirs)))

;;;###autoload
(defun mild-diff-dwim ()
  "Start mild-diff guessing directories from visible dired buffers.
Prompts for confirmation or selection."
  (interactive)
  (require 'dired-aux)
  (let* ((target-b (dired-dwim-target-directory))
         (target-a (if (eq major-mode 'dired-mode)
                       default-directory
                     (car (mild--get-dwim-dirs))))
         ;; If target-b is same as target-a, we only have one good candidate
         (has-two (and target-b (not (string= (expand-file-name target-a)
                                             (expand-file-name target-b)))))
         (left (read-directory-name "Left directory: " nil target-a t))
         (left-abs (directory-file-name (expand-file-name left)))
         ;; For right side, use target-b if it's different from what was chosen for left.
         ;; If not, don't suggest a default to avoid redundant picker population.
         (right-suggest (if (and target-b (not (string= left-abs (directory-file-name (expand-file-name target-b)))))
                            target-b
                          nil))
         (right (read-directory-name "Right directory: " 
                                     (or right-suggest (file-name-directory left-abs))
                                     right-suggest t))
         (right-abs (directory-file-name (expand-file-name right))))
    (if (string= left-abs right-abs)
        (user-error "Cannot compare a directory with itself: %s" left-abs)
      (mild-diff left-abs right-abs))))

(defface mild-same-face
  '((t :inherit font-lock-comment-face))
  "Face for files that are the same."
  :group 'mild)

(defface mild-diff-face
  '((t :inherit diff-changed :weight bold))
  "Face for files that are different."
  :group 'mild)

(defface mild-left-only-face
  '((t :inherit diff-added))
  "Face for files that exist only on the left side."
  :group 'mild)

(defface mild-right-only-face
  '((t :inherit diff-removed))
  "Face for files that exist only on the right side."
  :group 'mild)

(defface mild-header-face
  '((t :inherit font-lock-function-name-face :weight bold :underline t))
  "Face for the directory headers."
  :group 'mild)

(defface mild-dir-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for directory names (overrides diff-status faces)."
  :group 'mild)

(defface mild-filtered-face
  '((t :inherit shadow :strike-through t))
  "Face for filtered files."
  :group 'mild)

(defvar mild-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'mild-toggle-expand)
    (define-key map (kbd "RET") #'mild-action)
    (define-key map (kbd "e")   #'mild-action)
    (define-key map (kbd "=")   #'mild-action)
    (define-key map (kbd "f")   #'mild-find-file)
    (define-key map (kbd "v")   #'mild-find-file)
    (define-key map (kbd "g")   #'mild-refresh)
    (define-key map (kbd "G")   #'mild-refresh-full)
    (define-key map (kbd "n")   #'mild-next-line)
    (define-key map (kbd "p")   #'mild-previous-line)
    (define-key map (kbd "M-n") #'mild-next-change)
    (define-key map (kbd "M-p") #'mild-previous-change)
    (define-key map (kbd "o")   #'mild-jump-other-side)
    (define-key map (kbd "C")   #'mild-copy)
    (define-key map (kbd "w")   #'mild-copy-path-at-point)
    (define-key map (kbd "d")   #'mild-delete)
    (define-key map (kbd "SPC") #'mild-soft-diff)
    (define-key map (kbd "h")   #'mild-toggle-show-hidden)
    (define-key map (kbd "F")   #'mild-toggle-show-filtered)
    (define-key map (kbd "s")   #'mild-toggle-show-same)
    (define-key map (kbd "m")   #'mild-toggle-show-different)
    (define-key map (kbd "a")   #'mild-toggle-show-added)
    (define-key map (kbd "T")   #'mild-toggle-tree-lines)
    (define-key map (kbd ">")   #'mild-increase-indent)
    (define-key map (kbd "<")   #'mild-decrease-indent)
    (define-key map (kbd "C-c f") #'mild-toggle-filter-group)
    map)
  "Keymap for `mild-mode'.")

(define-derived-mode mild-mode special-mode "Mild"
  "Major mode for Meld-Inspired Local Diff."
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t)
  (make-local-variable 'mild-show-hidden)
  (make-local-variable 'mild-show-filtered)
  (make-local-variable 'mild-show-same)
  (make-local-variable 'mild-show-different)
  (make-local-variable 'mild-show-added)
  (setq-local revert-buffer-function #'mild-refresh)
  (mild--update-header-line))

(defun mild--move-to-node-name (on-right)
  "Move point to the node name on the current side (ON-RIGHT if non-nil).
If the current side is empty but the other has a file, snap to it."
  (let* ((width (or (bound-and-true-p mild--column-width) (/ (window-width) 2)))
         (limit (if on-right (line-end-position) (+ (line-beginning-position) width))))
    (if on-right
        (move-to-column width)
      (beginning-of-line))
    
    ;; Skip prefixes/icons/spaces on current side
    (let ((start-on-side (point)))
      (while (and (< (point) (line-end-position))
                  (< (point) limit)
                  (looking-at "[] └├│─\\[+-]"))
        (forward-char 1))
      
      ;; If we reached the limit on the left side, back up slightly to stay on the left
      (when (and (not on-right) (>= (current-column) width))
        (goto-char start-on-side)
        ;; Only skip tree chars, not all spaces
        (skip-chars-forward " └├│─")))
    
    ;; Smart snapping: only if this side is truly empty and the other side has a file
    (let ((node (get-text-property (point) 'mild-node)))
      (when (and node (mild-node-parent node)) ; Only snap for children, not root nodes
        (let* ((has-left (mild-node-left-path node))
               (has-right (mild-node-right-path node))
               (current-is-empty (not (if on-right has-right has-left))))
          (when (and current-is-empty (if on-right has-left has-right))
            (if on-right (beginning-of-line) (move-to-column width))
            (let ((new-limit (if on-right (+ (line-beginning-position) width) (line-end-position))))
              (while (and (< (point) (line-end-position))
                          (< (point) new-limit)
                          (looking-at "[] └├│─\\[+-]"))
                (forward-char 1)))))))))

(defun mild--change-status-p (node)
  "Return non-nil if NODE represents a change (different, added, or removed)."
  (memq (mild-node-diff-status node) '(different left-only right-only)))

(defun mild-next-change ()
  "Move to the next change (different, added, or removed node)."
  (interactive)
  (let* ((width (or (bound-and-true-p mild--column-width) (/ (window-width) 2)))
         (on-right (>= (current-column) width))
         (found nil)
         (orig (point)))
    (forward-line 1)
    (while (and (not found) (not (eobp)))
      (let ((node (get-text-property (point) 'mild-node)))
        (if (and node (mild--change-status-p node) (not (mild-node-is-dir node)))
            (setq found t)
          (forward-line 1))))
    (if found
        (mild--move-to-node-name on-right)
      (goto-char orig)
      (message "No next change"))))

(defun mild-previous-change ()
  "Move to the previous change (different, added, or removed node)."
  (interactive)
  (let* ((width (or (bound-and-true-p mild--column-width) (/ (window-width) 2)))
         (on-right (>= (current-column) width))
         (found nil)
         (orig (point)))
    (forward-line -1)
    (while (and (not found) (not (bobp)))
      (let ((node (get-text-property (point) 'mild-node)))
        (if (and node (mild--change-status-p node) (not (mild-node-is-dir node)))
            (setq found t)
          (forward-line -1))))
    (if found
        (mild--move-to-node-name on-right)
      (goto-char orig)
      (message "No previous change"))))

(defun mild-next-line ()
  "Move to the next line and align to node name."
  (interactive)
  (let* ((width (or (bound-and-true-p mild--column-width) (/ (window-width) 2)))
         (on-right (>= (current-column) width)))
    (forward-line 1)
    (mild--move-to-node-name on-right)))

(defun mild-previous-line ()
  "Move to the previous line and align to node name."
  (interactive)
  (let* ((width (or (bound-and-true-p mild--column-width) (/ (window-width) 2)))
         (on-right (>= (current-column) width)))
    (forward-line -1)
    (mild--move-to-node-name on-right)))

(defun mild-jump-other-side ()
  "Jump to the other side of the directory comparison."
  (interactive)
  (let* ((width (or (bound-and-true-p mild--column-width) (/ (window-width) 2)))
         (target-on-right (< (current-column) width)))
    (mild--move-to-node-name target-on-right)))

(defun mild-toggle-show-hidden ()
  "Toggle showing hidden files."
  (interactive)
  (setq mild-show-hidden (not mild-show-hidden))
  (message "Show hidden: %s" (if mild-show-hidden "on" "off"))
  (mild-refresh))

(defun mild-toggle-show-filtered ()
  "Toggle showing filtered files."
  (interactive)
  (setq mild-show-filtered (not mild-show-filtered))
  (message "Show filtered: %s" (if mild-show-filtered "on" "off"))
  (mild-refresh))

(defun mild-toggle-show-same ()
  "Toggle showing identical files."
  (interactive)
  (setq mild-show-same (not mild-show-same))
  (message "Show same: %s" (if mild-show-same "on" "off"))
  (mild--render))

(defun mild-toggle-tree-lines ()
  "Toggle the display of tree lines."
  (interactive)
  (setq mild-show-tree-lines (not mild-show-tree-lines))
  (mild--render))

(defun mild-increase-indent ()
  "Increase the directory indentation."
  (interactive)
  (setq mild-indent-offset (1+ mild-indent-offset))
  (message "Indent: %d" mild-indent-offset)
  (mild--render))

(defun mild-decrease-indent ()
  "Decrease the directory indentation."
  (interactive)
  (setq mild-indent-offset (max 1 (1- mild-indent-offset)))
  (message "Indent: %d" mild-indent-offset)
  (mild--render))

(defun mild-toggle-show-different ()
  "Toggle showing modified files."
  (interactive)
  (setq mild-show-different (not mild-show-different))
  (message "Show modified: %s" (if mild-show-different "on" "off"))
  (mild--render))

(defun mild-toggle-show-added ()
  "Toggle showing new files."
  (interactive)
  (setq mild-show-added (not mild-show-added))
  (message "Show new: %s" (if mild-show-added "on" "off"))
  (mild--render))

(defun mild-toggle-expand ()
  "Toggle expansion of the node at point."
  (interactive)
  (let ((node (get-text-property (point) 'mild-node)))
    (when (and node (mild-node-is-dir node))
      (setf (mild-node-expanded node) (not (mild-node-expanded node)))
      (let ((pos (point)))
        (mild--render)
        (goto-char pos)))))

(defun mild--stop-scan ()
  "Stop the currently running scanning thread if any."
  (when (and mild--scan-thread (thread-live-p mild--scan-thread))
    ;; Threads in Emacs are cooperative, so we can't force kill them easily,
    ;; but we can signal or just wait. For simplicity, we just clear the variable
    ;; so the callback won't update the UI for the old scan.
    (setq mild--scan-thread nil)))

(defun mild-refresh (&optional force-full)
  "Refresh the comparison asynchronously.
With non-nil FORCE-FULL, always use content comparison (diff -q)."
  (interactive "P")
  (mild--stop-scan)
  (let* ((buf (current-buffer))
         (left mild--left-root)
         (right mild--right-root)
         (root mild--root-node)
         (simple (if force-full nil mild-simple-comparison)))
    (setq mild--last-scan-mode (if simple 'simple 'full))
    (setq mild--scan-thread
          (make-thread
           (lambda ()
             (let ((mild-simple-comparison simple))
               (let ((new-children (mild--scan-dir left right root)))
                 ;; Use run-at-time to update UI from main thread context
                 (run-at-time 0 nil
                              (lambda (b nodes)
                                (when (buffer-live-p b)
                                  (with-current-buffer b
                                    (setf (mild-node-children mild--root-node) nodes)
                                    (setf (mild-node-diff-status mild--root-node)
                                          (if (cl-some (lambda (child)
                                                          (not (eq (mild-node-diff-status child) 'same)))
                                                        nodes)
                                              'different
                                            'same))
                                    (setq mild--scan-thread nil)
                                    (mild--update-header-line)
                                    (mild--render)
                                    (message "Scanning done."))))
                              buf new-children))))
           "mild-scan-thread"))
    (mild--update-header-line)
    (mild--render)
    (message (if force-full
                 "Full content scan in background..."
               "Rescanning directories in background..."))))

(defun mild-refresh-full ()
  "Force a full content-based scan (ignores `mild-simple-comparison')."
  (interactive)
  (mild-refresh t))

(defun mild--read-new-root (on-left)
  "Prompt for a new root directory for side ON-LEFT and update the buffer."
  (let* ((old-root (if on-left mild--left-root mild--right-root))
         (side-label (if on-left "Left" "Right"))
         (new-dir (read-directory-name
                   (format "%s directory: " side-label)
                   old-root nil t)))
    (unless (string= (directory-file-name (expand-file-name new-dir))
                     (directory-file-name old-root))
      (if on-left
          (progn
            (setq mild--left-root
                  (file-name-as-directory (expand-file-name new-dir)))
            (setq default-directory mild--left-root)
            (mild--apply-dir-locals))
        (setq mild--right-root
              (file-name-as-directory (expand-file-name new-dir))))
      (setf (if on-left
                (mild-node-left-path mild--root-node)
              (mild-node-right-path mild--root-node))
            (if on-left mild--left-root mild--right-root))
      (setf (mild-node-name mild--root-node)
            (file-name-nondirectory
             (directory-file-name mild--left-root)))
      (setf (mild-node-name-right mild--root-node)
            (file-name-nondirectory
             (directory-file-name mild--right-root)))
      (mild-refresh))))

(defun mild-action ()
  "Perform default action on node (ediff or view).

On the header line (directory paths), prompt to replace the
current side's directory with a new one via completion.
On files, run ediff or open.  On directories, open in dired."
  (interactive)
  (let* ((node (get-text-property (point) 'mild-node))
         (header (get-text-property (point) 'mild-header)))
    (cond
     (header
      (let ((cur-col (current-column)))
        (mild--read-new-root (< cur-col mild--column-width))))
     (node
      (let* ((cur-col (current-column))
             (on-left (< cur-col mild--column-width))
             (left (mild-node-left-path node))
             (right (mild-node-right-path node))
             (side-path (if on-left left right)))
        (cond ((and left right (not (mild-node-is-dir node)))
               (if (eq (mild-node-diff-status node) 'same)
                   (find-file-other-window side-path)
                 (ediff-files left right)))
              (side-path (find-file-other-window side-path))
              (t (message "No file on %s side"
                          (if on-left "left" "right")))))))))

(defun mild-find-file ()
  "Open the file on the current side in another window."
  (interactive)
  (let* ((node (get-text-property (point) 'mild-node))
         (cur-col (current-column)))
    (when node
      (let ((path (if (< cur-col mild--column-width)
                      (mild-node-left-path node)
                    (mild-node-right-path node))))
        (if path
            (find-file-other-window path)
          (message "No file on %s side" (if (< cur-col mild--column-width) "left" "right")))))))

(defun mild-soft-diff ()
  "Contextual action for SPC.
If on a directory, toggle expansion.
If on a file, show a quick diff of the current node."
  (interactive)
  (let ((node (get-text-property (point) 'mild-node)))
    (if (not node)
        (message "No node at point")
      (if (mild-node-is-dir node)
          (mild-toggle-expand)
        (let ((left (mild-node-left-path node))
              (right (mild-node-right-path node)))
          (if (and left right (not (file-directory-p left)) (not (file-directory-p right)))
              (diff left right)
            (message "Soft diff only works for comparing two files")))))))

(defun mild-copy-path-at-point (arg)
  "Copy the filename at point to the kill ring.
With prefix ARG, copy the full absolute path.
The side is determined by the current column."
  (interactive "P")
  (let* ((node (get-text-property (point) 'mild-node))
         (cur-col (current-column)))
    (if (not node)
        (message "No node at point")
      (let* ((path (if (< cur-col mild--column-width)
                       (mild-node-left-path node)
                     (mild-node-right-path node)))
             (text (if arg path (mild-node-name node))))
        (if text
            (progn
              (kill-new text)
              (message "Copied: %s" text))
          (message "No file on this side"))))))

(defun mild-copy ()
  "Copy the file/dir at point to the other side.
Creates missing parent directories on the destination side if needed."
  (interactive)
  (let* ((node (get-text-property (point) 'mild-node))
         (cur-col (current-column))
         (on-left (< cur-col mild--column-width)))
    (unless node (user-error "No node at point"))
    (let* ((source (if on-left (mild-node-left-path node) (mild-node-right-path node)))
           (source-root (if on-left mild--left-root mild--right-root))
           (dest-root (if on-left mild--right-root mild--left-root)))
      (unless source (user-error "No file on current side to copy"))
      (let* ((rel-path (file-relative-name source source-root))
             (dest (directory-file-name (expand-file-name rel-path dest-root)))
             (dest-parent (file-name-directory dest)))
        (when (yes-or-no-p (format "Copy %s to %s?" (mild-node-name node) dest-root))
          (condition-case err
              (progn
                ;; Ensure dest-parent is a directory
                (unless (file-directory-p dest-parent)
                  (when (file-exists-p dest-parent) (delete-file dest-parent))
                  (make-directory dest-parent t))
                ;; Perform the copy
                (if (file-directory-p source)
                    ;; For directories, if dest exists, we must handle it to avoid nesting
                    (if (fboundp 'copy-directory-contents) ; Hypothetical, but let's use the real args
                        (copy-directory source dest t t t) ; Emacs 28+
                      ;; Emacs 27 fallback: remove dest if it exists as a file
                      (when (and (file-exists-p dest) (not (file-directory-p dest)))
                        (delete-file dest))
                      (copy-directory source dest t t))
                  (copy-file source dest t))
                (mild-refresh)
                (message "Copied %s" (mild-node-name node)))
            (error (message "Copy failed: %s" (error-message-string err)))))))))

(defun mild-delete ()
  "Delete the file/directory at point from the current side."
  (interactive)
  (let* ((node (get-text-property (point) 'mild-node))
         (cur-col (current-column))
         (on-left (< cur-col mild--column-width)))
    (unless node (user-error "No node at point"))
    (let* ((path (if on-left (mild-node-left-path node) (mild-node-right-path node)))
           (side (if on-left "left" "right")))
      (unless path (user-error "No file on %s side to delete" side))
      (when (yes-or-no-p (format "Delete %s from %s side?" (mild-node-name node) side))
        (condition-case err
            (progn
              (if (file-directory-p path)
                  (delete-directory path t t)
                (delete-file path t))
              (mild-refresh)
              (message "Deleted %s from %s side" (mild-node-name node) side))
          (error (message "Delete failed: %s" (error-message-string err))))))))

(defun mild--toggle-indicator (label enabled)
  "Return a propertized string for toggle LABEL showing ENABLED state."
  (propertize label 'face (if enabled 'bold 'shadow)))

(defun mild--display-state-string ()
  "Return a compact string showing current display toggle states."
  (let ((groups (mapcar (lambda (g) (concat "[" g "]"))
                        mild--active-filter-groups)))
    (concat
     (mild--toggle-indicator "h" mild-show-hidden)
     (mild--toggle-indicator "f" mild-show-filtered)
     (mild--toggle-indicator "s" mild-show-same)
     (mild--toggle-indicator "m" mild-show-different)
     (mild--toggle-indicator "a" mild-show-added)
     (mild--toggle-indicator "T" mild-show-tree-lines)
     (when groups
       (concat " " (mapconcat #'identity groups " "))))))

(defun mild--update-header-line ()
  "Update the header line with legend and status."
  (setq header-line-format
        '(:eval
          (let ((scan-msg (if (and (boundp 'mild--scan-thread) 
                                    mild--scan-thread 
                                    (thread-live-p mild--scan-thread))
                               (propertize " [SCANNING...] " 'face 'highlight)
                             ""))
                (mode-msg (if (eq mild--last-scan-mode 'simple)
                               (propertize " [SIMPLE] " 'face 'warning)
                             (propertize " [FULL] " 'face 'success))))
            (concat
             scan-msg mode-msg
              " [" (mild--display-state-string) "] ")))))


(defun mild--render ()
  "Render the tree in the current buffer, attempting to preserve point."
  (let ((inhibit-read-only t)
        (old-node (get-text-property (point) 'mild-node))
        (old-col (current-column))
        (target-path nil))
    ;; Save path of current node to restore point later
    (when old-node
      (setq target-path (or (mild-node-left-path old-node)
                           (mild-node-right-path old-node))))
    
    (setq mild--column-width (/ (window-width) 2))
    (erase-buffer)
    ;; Headers (with mild-header property for interactive directory switching)
    (let ((header-start (point)))
      (insert (propertize (truncate-string-to-width (abbreviate-file-name mild--left-root)
                                                    mild--column-width nil ?\s)
                          'face 'mild-header-face))
      (insert (propertize (truncate-string-to-width (abbreviate-file-name mild--right-root)
                                                    mild--column-width nil ?\s)
                          'face 'mild-header-face))
      (add-text-properties header-start (point) '(mild-header t mouse-face highlight)))
    (insert "\n" (make-string (* 2 mild--column-width) ?-) "\n")
    ;; Root node (collapsible)
    (mild--render-nodes (list mild--root-node) "" mild--column-width)
    
    ;; Restore point
    (if target-path
        (let ((found nil))
          (goto-char (point-min))
          (while (and (not found) (not (eobp)))
            (let ((node (get-text-property (point) 'mild-node)))
              (if (and node (or (string= (mild-node-left-path node) target-path)
                                (string= (mild-node-right-path node) target-path)))
                  (progn
                    (setq found t)
                    (move-to-column old-col))
                (forward-line 1))))
          (unless found (goto-char (point-min))))
      (goto-char (point-min)))))

(defun mild--node-visible-p (node)
  "Determine if NODE should be visible based on status filters.
The root node is always visible."
  (or (eq node mild--root-node)
      (let ((status (mild-node-diff-status node)))
        (cond ((eq status 'same) mild-show-same)
              ((eq status 'different) mild-show-different)
              ((memq status '(left-only right-only)) mild-show-added)
              (t t)))))

(defun mild--render-nodes (nodes indent-prefix column-width)
  "Render a list of NODES with INDENT-PREFIX and COLUMN-WIDTH."
  (let ((count (length nodes))
        (idx 0))
    (dolist (node nodes)
      (setq idx (1+ idx))
      (when (mild--node-visible-p node)
        (let* ((status (mild-node-diff-status node))
               (is-last (= idx count))
               (branch (if mild-show-tree-lines
                           (concat (if is-last "└" "├")
                                   (make-string (1- mild-indent-offset) ?─))
                         (make-string mild-indent-offset ?\s)))
               (full-prefix (concat indent-prefix branch))
               (face (cond ((eq status 'filtered) 'mild-filtered-face)
                           ((eq status 'same) 'mild-same-face)
                           ((eq status 'different) 'mild-diff-face)
                           ((eq status 'left-only) 'mild-left-only-face)
                           ((eq status 'right-only) 'mild-right-only-face)))
               (name-face (if (mild-node-is-dir node) 'mild-dir-face face))
               (dir-indicator (if (mild-node-is-dir node)
                                    (concat (if (mild-node-expanded node) "[-] " "[+] ")
                                            mild-dir-prefix)
                                  "")))
           (let ((start (point)))
             ;; Left Side
             (insert (propertize full-prefix 'face 'shadow))
             (when (mild-node-left-path node)
               (insert dir-indicator)
               (insert (propertize (mild-node-name node) 'face name-face)))
            
            ;; Padding to Right Side
            (let ((current-len (current-column)))
              (if (< current-len column-width)
                  (insert (make-string (- column-width current-len) ?\s))
                (insert "  ")))
            
            ;; Right Side
            (insert (propertize full-prefix 'face 'shadow))
            (when (mild-node-right-path node)
              (insert dir-indicator)
               (insert (propertize (or (mild-node-name-right node)
                                      (mild-node-name node))
                                   'face name-face)))
            
            (add-text-properties start (point) `(mild-node ,node mouse-face highlight))
            (insert "\n")
            
            ;; Recurse
            (when (and (mild-node-expanded node) (mild-node-children node))
              (mild--render-nodes (mild-node-children node)
                                  (if mild-show-tree-lines
                                      (concat indent-prefix 
                                              (if is-last " " "│")
                                              (make-string (1- mild-indent-offset) ?\s))
                                    (concat indent-prefix (make-string mild-indent-offset ?\s)))
                                   column-width))))))))

(transient-define-prefix mild-dispatch ()
   "Transient menu for Mild."
    [["File status"
     ("s" (lambda () (if mild-show-same "✓ same" "  same"))
      (lambda () (interactive) (mild-toggle-show-same)) :transient t)
     ("m" (lambda () (if mild-show-different "✓ modified" "  modified"))
      (lambda () (interactive) (mild-toggle-show-different)) :transient t)
     ("a" (lambda () (if mild-show-added "✓ added" "  added"))
      (lambda () (interactive) (mild-toggle-show-added)) :transient t)]
    ["Filename"
     :if (lambda () mild-filter-groups)
     :class transient-column
     :setup-children
     (lambda (&rest _)
       (when mild-filter-groups
         (let ((keys '("z" "x" "y" "w" "v" "u" "t" "r" "q" "j"))
               (suffixes nil))
           (dolist (g mild-filter-groups)
             (let* ((name (car g))
                    (key (pop keys)))
               (unless (fboundp (intern (format "mild--toggle-group-%s" (downcase name))))
                  (let ((n name))
                    (defalias (intern (format "mild--toggle-group-%s" (downcase name)))
                      (lambda ()
                        (interactive)
                        (mild-toggle-filter-group n)
                        (run-at-time 0 nil
                         (lambda () (transient-setup 'mild-dispatch))))
                      (format "Toggle filter group `%s'." (downcase name)))))
               (push (transient-parse-suffix
                      'mild-dispatch
                      (list key
                            (format "%s %s"
                                    (if (member name mild--active-filter-groups) "✓" "  ")
                                    name)
                             (intern (format "mild--toggle-group-%s" (downcase name)))
                             :transient t))
                     suffixes)))
           (nreverse suffixes))))]
    ["Toggles"
     ("h" (lambda () (if mild-show-hidden "✓ hidden" "  hidden"))
      (lambda () (interactive) (mild-toggle-show-hidden)) :transient t)
     ("F" (lambda () (if mild-show-filtered "✓ filtered" "  filtered"))
      (lambda () (interactive) (mild-toggle-show-filtered)) :transient t)
     ("T" (lambda () (if mild-show-tree-lines "✓ tree-lines" "  tree-lines"))
      (lambda () (interactive) (mild-toggle-tree-lines)) :transient t)]
    ["Actions"
     ("g"   "rescan (simple)"    mild-refresh)
     ("G"   "rescan (full diff)" mild-refresh-full)
     ("o"   "jump other side"    mild-jump-other-side :transient t)
     ("M-n" "next change"        mild-next-change :transient t)
     ("M-p" "previous change"    mild-previous-change :transient t)
     ("SPC" "soft diff / fold"   mild-soft-diff)
     ("C"   "copy to other side" mild-copy)
     ("d"   "delete from side"   mild-delete)
     ("w"   "copy path"          mild-copy-path-at-point)
     ("RET" "action (ediff)"     mild-action)]
    ["Indent"
     (">" "more"  mild-increase-indent :transient t)
     ("<" "less" mild-decrease-indent :transient t)]])

(define-key mild-mode-map (kbd "?") #'mild-dispatch)

(provide 'mild)
;;; mild.el ends here
