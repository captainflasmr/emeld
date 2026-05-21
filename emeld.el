;;; emeld.el --- Meld-Inspired Local Diff -*- lexical-binding: t; -*-

;; Keywords: files, tools
;; Version: 0.9.3

;;; Commentary:
;; A simple side-by-side directory comparison tool inspired by Meld.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'transient)

(defgroup emeld nil
  "Meld-Inspired Local Diff."
  :group 'tools)

(defcustom emeld-filter-groups
  '(("Backups" t "#*#" ".#*" "~*" "*~" "*.orig" "*.bak" "*.swp")
    ("OS metadata" t ".DS_Store" "._*" ".Spotlight-V100" ".Trashes"
     "Thumbs.db" "Desktop.ini")
    ("Version Control" t ".git" ".svn" ".hg" "CVS" "_darcs" ".bzr"
     "_MTN" ".fslckout" "_FOSSIL_" ".fos" ".osc")
    ("Binaries" t "*.pyc" "*.a" "*.obj" "*.o" "*.so" "*.la" "*.lib"
     "*.dll" "*.exe")
    ("Media" nil "*.jpg" "*.gif" "*.png" "*.bmp" "*.wav" "*.mp3"
     "*.ogg" "*.flac" "*.avi" "*.mpg" "*.xcf" "*.xpm"))
  "List of named filter groups.
Each entry is (NAME [ACTIVE] GLOBS...).  NAME is a string.
If the second element is t or nil, it sets the default active state;
otherwise all elements after NAME are glob patterns and the group
defaults to inactive.  Toggle groups on/off with `emeld-toggle-filter-group'."
  :type '(repeat (list (string :tag "Group name")
                       (repeat :tag "Glob patterns" string)))
  :group 'emeld)

(put 'emeld-filter-groups 'safe-local-variable
     (lambda (val)
       (and (listp val)
            (cl-every (lambda (entry)
                        (and (listp entry) (stringp (car entry))
                             (cl-every #'stringp (cdr entry))))
                      val))))

(defcustom emeld-show-filtered nil
  "Whether to show filtered files (matching active filter groups)."
  :type 'boolean
  :group 'emeld)

(defcustom emeld-show-same nil
  "Whether to show identical files."
  :type 'boolean
  :group 'emeld)

(defcustom emeld-show-different t
  "Whether to show modified files."
  :type 'boolean
  :group 'emeld)

(defcustom emeld-show-added t
  "Whether to show new (orphan) files."
  :type 'boolean
  :group 'emeld)

(defcustom emeld-dir-prefix "📁 "
  "Prefix string shown before directory names to distinguish from files.
Set to \"\" for no prefix."
  :type 'string
  :group 'emeld)

(defcustom emeld-indent-offset 2
  "Indentation offset for each directory level."
  :type 'integer
  :group 'emeld)

(defcustom emeld-show-tree-lines t
  "Whether to show hierarchical tree lines."
  :type 'boolean
  :group 'emeld)

(defcustom emeld-simple-comparison nil
  "If non-nil, only compare file sizes for diff status.
This is much faster but less accurate than content comparison."
  :type 'boolean
  :group 'emeld)

(defcustom emeld-column-ratio 50
  "Width of the left column as a percentage of the window width (10-90).
Defaults to 50 for equal-width panes."
  :type 'integer
  :group 'emeld)

(defun emeld--apply-dir-locals ()
  "Apply .dir-locals.el from `default-directory' to the current buffer."
  (hack-dir-local-variables-non-file-buffer))

(cl-defstruct (emeld-node
               (:constructor emeld-node-create))
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

(defvar-local emeld--scan-thread nil
  "Current scanning thread for this buffer.")
(defvar-local emeld--last-scan-mode 'normal
  "Scan mode used for the last refresh: `simple' or `full'.")
(defvar-local emeld--active-filter-groups nil
  "List of currently active filter group names.")
(defvar-local emeld--right-root nil)
(defvar-local emeld--root-node nil)
(defvar-local emeld--column-width nil)

(defun emeld--glob-to-regexp (glob)
  "Convert GLOB to a regular expression.
If GLOB contains a slash, it matches against the relative path.
Otherwise, it matches against the filename."
  (let ((rx (wildcard-to-regexp glob)))
    (if (string-match-p "/" glob)
        rx
      (concat "^" rx "$"))))

(defun emeld--group-globs (group)
  "Return the list of glob patterns from filter GROUP entry."
  (let ((rest (cdr group)))
    (if (booleanp (car rest)) (cdr rest) rest)))

(defun emeld--group-active-default (group)
  "Return the default active state for filter GROUP."
  (let ((rest (cdr group)))
    (and (booleanp (car rest)) (car rest))))

(defun emeld--filtered-p (name rel-path)
  "Check if NAME or REL-PATH is filtered.
Check active filter groups."
  (or (cl-some (lambda (group)
                 (when (member (car group) emeld--active-filter-groups)
                   (cl-some (lambda (glob)
                              (let ((rx (emeld--glob-to-regexp glob)))
                                (if (string-match-p "/" glob)
                                    (string-match-p rx rel-path)
                                  (string-match-p rx name))))
                            (emeld--group-globs group))))
               emeld-filter-groups)))

(defun emeld-toggle-filter-group (group)
  "Toggle a named filter GROUP on or off.
GROUP is the name of a group defined in `emeld-filter-groups'."
  (interactive
   (let ((names (mapcar #'car emeld-filter-groups)))
     (unless names (user-error "No filter groups defined (customize `emeld-filter-groups')"))
     (list (completing-read "Toggle filter group: " names nil t))))
  (if (member group emeld--active-filter-groups)
      (progn
        (setq emeld--active-filter-groups (delete group emeld--active-filter-groups))
        (message "Filter group \"%s\" disabled" group))
    (push group emeld--active-filter-groups)
    (message "Filter group \"%s\" enabled" group))
   (emeld-refresh))

(defun emeld-list-filter-groups ()
  "Display all filter groups and their glob patterns in an org-mode buffer."
  (interactive)
  (let ((active-groups emeld--active-filter-groups))
    (with-current-buffer (get-buffer-create "*emeld filter groups*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (setq-local buffer-read-only t)
        (insert "#+TITLE: Emeld Filter Groups\n\n"
                "* Overview\n"
                "Filter groups are toggled from the transient menu (~ca~, ~cb~, ...) or with ~C-c f~.\n\n")
        (dolist (g emeld-filter-groups)
          (let ((name (car g))
                (active (member (car g) active-groups))
                (globs (emeld--group-globs g)))
            (insert (format "* %s  %s\n"
                            name
                            (if active "[x]" "[ ]")))
            (dolist (glob globs)
              (insert (format "  - ~%s~\n" glob)))
            (insert "\n")))
        (goto-char (point-min))))
    (pop-to-buffer "*emeld filter groups*")))

(defun emeld--get-files (dir)
  "Get all files in DIR except . and .."
  (when (and dir (file-directory-p dir))
    (directory-files dir t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))

(defun emeld--compare-files (left right)
  "Compare LEFT and RIGHT files. Return 'same or 'different."
  (if (and left right
           (file-exists-p left)
           (file-exists-p right))
      (let ((is-dir-left (file-directory-p left))
            (is-dir-right (file-directory-p right)))
        (cond ((not (eq is-dir-left is-dir-right)) 'different)
              (is-dir-left 'same) ; Directories are 'same' by default, children will update it
              (t (if (if emeld-simple-comparison
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

(defun emeld--scan-dir (left-dir right-dir &optional parent rel-path)
  "Recursively scan LEFT-DIR and RIGHT-DIR to build the tree.
PARENT is the parent `emeld-node`. REL-PATH is the relative path from root."
  (let* ((all-names (delete-dups
                      (append (mapcar #'file-name-nondirectory (emeld--get-files left-dir))
                              (mapcar #'file-name-nondirectory (emeld--get-files right-dir)))))
          (nodes nil)
          (yield-count 0))
    (dolist (name (sort all-names #'string<))
      (setq yield-count (1+ yield-count))
      (when (= 0 (mod yield-count 10))
        (sleep-for 0.001))
      (let* ((left (and left-dir (expand-file-name name left-dir)))
             (right (and right-dir (expand-file-name name right-dir)))
             (exists-left (and left (file-exists-p left)))
             (exists-right (and right (file-exists-p right)))
             (this-rel-path (if rel-path (concat rel-path "/" name) name))
             (is-dir (or (and exists-left (file-directory-p left))
                         (and exists-right (file-directory-p right))))
             (status (emeld--compare-files (if exists-left left nil) (if exists-right right nil)))
             (is-filtered (emeld--filtered-p name this-rel-path)))
        
        (when (or (not is-filtered)
                  emeld-show-filtered)
          (let ((node (emeld-node-create :name name
                                       :left-path (if exists-left left nil)
                                       :right-path (if exists-right right nil)
                                       :parent parent
                                       :is-dir is-dir
                                       :diff-status (if is-filtered 'filtered status)
                                       :is-filtered is-filtered)))
            (when is-dir
              (setf (emeld-node-children node)
                    (emeld--scan-dir (if (and exists-left is-dir) left nil)
                                    (if (and exists-right is-dir) right nil)
                                    node
                                    this-rel-path))
              ;; Update directory status based on children
              (when (and (not is-filtered) (eq (emeld-node-diff-status node) 'same))
                (when (cl-some (lambda (child) (not (eq (emeld-node-diff-status child) 'same)))
                               (emeld-node-children node))
                  (setf (emeld-node-diff-status node) 'different))))
            (push node nodes)))))
    (nreverse nodes)))

(defun emeld-diff (dir1 dir2)
  "Compare DIR1 and DIR2 side-by-side."
  (interactive "DLeft directory: \nDRight directory: ")
  (let ((buf (get-buffer-create (format "*emeld: %s <-> %s*" 
                                        (file-name-nondirectory (directory-file-name dir1))
                                        (file-name-nondirectory (directory-file-name dir2))))))
    (with-current-buffer buf
      (emeld-mode)
      (setq default-directory (file-name-as-directory (expand-file-name dir1)))
      (emeld--apply-dir-locals)
      (setq emeld--active-filter-groups
            (cl-loop for g in emeld-filter-groups
                     when (emeld--group-active-default g)
                     collect (car g)))
      (setq emeld--left-root (file-name-as-directory (expand-file-name dir1)))
      (setq emeld--right-root (file-name-as-directory (expand-file-name dir2)))
      (setq emeld--last-scan-mode (if emeld-simple-comparison 'fast 'normal))
      (setq emeld--root-node (emeld-node-create
                              :name (file-name-nondirectory
                                     (directory-file-name emeld--left-root))
                              :name-right (file-name-nondirectory
                                           (directory-file-name emeld--right-root))
                              :left-path emeld--left-root
                              :right-path emeld--right-root
                              :is-dir t
                              :expanded t))
      (setf (emeld-node-children emeld--root-node) nil)
      (emeld--render))
    (switch-to-buffer buf)
    (emeld-refresh)))

(defun emeld--get-dwim-dirs ()
  "Try to guess directories based on visible dired buffers."
  (let ((dired-dirs (delq nil
                          (mapcar (lambda (w)
                                    (with-current-buffer (window-buffer w)
                                      (when (eq major-mode 'dired-mode)
                                        (directory-file-name (expand-file-name default-directory)))))
                                  (window-list)))))
    (delete-dups dired-dirs)))

;;;###autoload
(defun emeld-diff-dwim ()
  "Start emeld-diff guessing directories from visible dired buffers.
Prompts for confirmation or selection."
  (interactive)
  (require 'dired-aux)
  (let* ((target-b (dired-dwim-target-directory))
         (target-a (if (eq major-mode 'dired-mode)
                       default-directory
                     (car (emeld--get-dwim-dirs))))
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
      (emeld-diff left-abs right-abs))))

(defface emeld-same-face
  '((t :inherit font-lock-comment-face))
  "Face for files that are the same."
  :group 'emeld)

(defface emeld-diff-face
  '((t :inherit diff-changed :weight bold))
  "Face for files that are different."
  :group 'emeld)

(defface emeld-left-only-face
  '((t :inherit diff-added))
  "Face for files that exist only on the left side."
  :group 'emeld)

(defface emeld-right-only-face
  '((t :inherit diff-removed))
  "Face for files that exist only on the right side."
  :group 'emeld)

(defface emeld-header-face
  '((t :inherit font-lock-function-name-face :weight bold :underline t))
  "Face for the directory headers."
  :group 'emeld)

(defface emeld-dir-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for directory names (overrides diff-status faces)."
  :group 'emeld)

(defface emeld-filtered-face
  '((t :inherit shadow :strike-through t))
  "Face for filtered files."
  :group 'emeld)

(defvar emeld-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'emeld-toggle-expand)
    (define-key map (kbd "RET") #'emeld-action)
    (define-key map (kbd "e")   #'emeld-action)
    (define-key map (kbd "=")   #'emeld-action)
    (define-key map (kbd "f")   #'emeld-find-file)
    (define-key map (kbd "v")   #'emeld-find-file)
    (define-key map (kbd "g")   #'emeld-refresh)
    (define-key map (kbd "G")   #'emeld-refresh-full)
    (define-key map (kbd "n")   #'emeld-next-line)
    (define-key map (kbd "p")   #'emeld-previous-line)
    (define-key map (kbd "M-n") #'emeld-next-change)
    (define-key map (kbd "M-p") #'emeld-previous-change)
    (define-key map (kbd "o")   #'emeld-jump-other-side)
    (define-key map (kbd "P")   #'emeld-format-patch)
    (define-key map (kbd "C")   #'emeld-copy)
    (define-key map (kbd "w")   #'emeld-copy-path-at-point)
    (define-key map (kbd "d")   #'emeld-delete)
    (define-key map (kbd "SPC") #'emeld-soft-diff)
    (define-key map (kbd "F")   #'emeld-toggle-show-filtered)
    (define-key map (kbd "s")   #'emeld-toggle-show-same)
    (define-key map (kbd "m")   #'emeld-toggle-show-different)
    (define-key map (kbd "a")   #'emeld-toggle-show-added)
    (define-key map (kbd "T")   #'emeld-toggle-tree-lines)
    (define-key map (kbd ">")   #'emeld-increase-indent)
    (define-key map (kbd "<")   #'emeld-decrease-indent)
    (define-key map (kbd "}")   #'emeld-grow-column)
    (define-key map (kbd "{")   #'emeld-shrink-column)
    (define-key map (kbd "C-c f") #'emeld-toggle-filter-group)
    map)
  "Keymap for `emeld-mode'.")

(define-derived-mode emeld-mode special-mode "Emeld"
  "Major mode for Meld-Inspired Local Diff."
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t)
  (make-local-variable 'emeld-show-filtered)
  (make-local-variable 'emeld-show-same)
  (make-local-variable 'emeld-show-different)
  (make-local-variable 'emeld-show-added)
  (setq-local revert-buffer-function #'emeld-refresh)
  (emeld--update-header-line))

(defun emeld--move-to-node-name (on-right)
  "Move point to the node name on the current side (ON-RIGHT if non-nil).
If the current side is empty but the other has a file, snap to it."
  (let* ((width (or (bound-and-true-p emeld--column-width) (/ (* (window-width) emeld-column-ratio) 100)))
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
    (let ((node (get-text-property (point) 'emeld-node)))
      (when (and node (emeld-node-parent node)) ; Only snap for children, not root nodes
        (let* ((has-left (emeld-node-left-path node))
               (has-right (emeld-node-right-path node))
               (current-is-empty (not (if on-right has-right has-left))))
          (when (and current-is-empty (if on-right has-left has-right))
            (if on-right (beginning-of-line) (move-to-column width))
            (let ((new-limit (if on-right (+ (line-beginning-position) width) (line-end-position))))
              (while (and (< (point) (line-end-position))
                          (< (point) new-limit)
                          (looking-at "[] └├│─\\[+-]"))
                (forward-char 1)))))))))

(defun emeld--change-status-p (node)
  "Return non-nil if NODE represents a change (different, added, or removed)."
  (memq (emeld-node-diff-status node) '(different left-only right-only)))

(defun emeld-next-change ()
  "Move to the next change (different, added, or removed node)."
  (interactive)
  (let* ((width (or (bound-and-true-p emeld--column-width) (/ (* (window-width) emeld-column-ratio) 100)))
         (on-right (>= (current-column) width))
         (found nil)
         (orig (point)))
    (forward-line 1)
    (while (and (not found) (not (eobp)))
      (let ((node (get-text-property (point) 'emeld-node)))
        (if (and node (emeld--change-status-p node) (not (emeld-node-is-dir node)))
            (setq found t)
          (forward-line 1))))
    (if found
        (emeld--move-to-node-name on-right)
      (goto-char orig)
      (message "No next change"))))

(defun emeld-previous-change ()
  "Move to the previous change (different, added, or removed node)."
  (interactive)
  (let* ((width (or (bound-and-true-p emeld--column-width) (/ (* (window-width) emeld-column-ratio) 100)))
         (on-right (>= (current-column) width))
         (found nil)
         (orig (point)))
    (forward-line -1)
    (while (and (not found) (not (bobp)))
      (let ((node (get-text-property (point) 'emeld-node)))
        (if (and node (emeld--change-status-p node) (not (emeld-node-is-dir node)))
            (setq found t)
          (forward-line -1))))
    (if found
        (emeld--move-to-node-name on-right)
      (goto-char orig)
      (message "No previous change"))))

(defun emeld-next-line ()
  "Move to the next line and align to node name."
  (interactive)
  (let* ((width (or (bound-and-true-p emeld--column-width) (/ (* (window-width) emeld-column-ratio) 100)))
         (on-right (>= (current-column) width)))
    (forward-line 1)
    (emeld--move-to-node-name on-right)))

(defun emeld-previous-line ()
  "Move to the previous line and align to node name."
  (interactive)
  (let* ((width (or (bound-and-true-p emeld--column-width) (/ (* (window-width) emeld-column-ratio) 100)))
         (on-right (>= (current-column) width)))
    (forward-line -1)
    (emeld--move-to-node-name on-right)))

(defun emeld-jump-other-side ()
  "Jump to the other side of the directory comparison."
  (interactive)
  (let* ((width (or (bound-and-true-p emeld--column-width) (/ (* (window-width) emeld-column-ratio) 100)))
         (target-on-right (< (current-column) width)))
    (emeld--move-to-node-name target-on-right)))

(defun emeld-toggle-show-filtered ()
  "Toggle showing filtered files."
  (interactive)
  (setq emeld-show-filtered (not emeld-show-filtered))
  (message "Show filtered: %s" (if emeld-show-filtered "on" "off"))
  (emeld-refresh))

(defun emeld-toggle-show-same ()
  "Toggle showing identical files."
  (interactive)
  (setq emeld-show-same (not emeld-show-same))
  (message "Show same: %s" (if emeld-show-same "on" "off"))
  (emeld--render))

(defun emeld-toggle-tree-lines ()
  "Toggle the display of tree lines."
  (interactive)
  (setq emeld-show-tree-lines (not emeld-show-tree-lines))
  (emeld--render))

(defun emeld-increase-indent ()
  "Increase the directory indentation."
  (interactive)
  (setq emeld-indent-offset (1+ emeld-indent-offset))
  (message "Indent: %d" emeld-indent-offset)
  (emeld--render))

(defun emeld-decrease-indent ()
  "Decrease the directory indentation."
  (interactive)
  (setq emeld-indent-offset (max 1 (1- emeld-indent-offset)))
  (message "Indent: %d" emeld-indent-offset)
  (emeld--render))

(defun emeld-shrink-column ()
  "Shrink the left column by 5%."
  (interactive)
  (setq emeld-column-ratio (max 10 (- emeld-column-ratio 5)))
  (message "Col %d/%d" emeld-column-ratio (- 100 emeld-column-ratio))
  (emeld--render))

(defun emeld-grow-column ()
  "Grow the left column by 5%."
  (interactive)
  (setq emeld-column-ratio (min 90 (+ emeld-column-ratio 5)))
  (message "Col %d/%d" emeld-column-ratio (- 100 emeld-column-ratio))
  (emeld--render))

(defun emeld-toggle-show-different ()
  "Toggle showing modified files."
  (interactive)
  (setq emeld-show-different (not emeld-show-different))
  (message "Show modified: %s" (if emeld-show-different "on" "off"))
  (emeld--render))

(defun emeld-toggle-show-added ()
  "Toggle showing new files."
  (interactive)
  (setq emeld-show-added (not emeld-show-added))
  (message "Show new: %s" (if emeld-show-added "on" "off"))
  (emeld--render))

(defun emeld-toggle-expand ()
  "Toggle expansion of the node at point."
  (interactive)
  (let ((node (get-text-property (point) 'emeld-node)))
    (when (and node (emeld-node-is-dir node))
      (setf (emeld-node-expanded node) (not (emeld-node-expanded node)))
      (let ((pos (point)))
        (emeld--render)
        (goto-char pos)))))

(defun emeld--stop-scan ()
  "Stop the currently running scanning thread if any."
  (when (and emeld--scan-thread (thread-live-p emeld--scan-thread))
    ;; Threads in Emacs are cooperative, so we can't force kill them easily,
    ;; but we can signal or just wait. For simplicity, we just clear the variable
    ;; so the callback won't update the UI for the old scan.
    (setq emeld--scan-thread nil)))

(defun emeld-refresh (&optional force-full)
  "Refresh the comparison asynchronously.
With non-nil FORCE-FULL, always use content comparison (diff -q)."
  (interactive "P")
  (emeld--stop-scan)
  (let* ((buf (current-buffer))
         (left emeld--left-root)
         (right emeld--right-root)
         (root emeld--root-node)
         (simple (if force-full nil emeld-simple-comparison)))
    (setq emeld--last-scan-mode (if simple 'fast 'normal))
    (setq emeld--scan-thread
          (make-thread
           (lambda ()
             (let ((emeld-simple-comparison simple))
               (let ((new-children (emeld--scan-dir left right root)))
                 ;; Use run-at-time to update UI from main thread context
                 (run-at-time 0 nil
                              (lambda (b nodes)
                                (when (buffer-live-p b)
                                  (with-current-buffer b
                                    (setf (emeld-node-children emeld--root-node) nodes)
                                    (setf (emeld-node-diff-status emeld--root-node)
                                          (if (cl-some (lambda (child)
                                                          (not (eq (emeld-node-diff-status child) 'same)))
                                                        nodes)
                                              'different
                                            'same))
                                    (setq emeld--scan-thread nil)
                                    (emeld--update-header-line)
                                    (emeld--render)
                                    (message "Scanning done."))))
                              buf new-children))))
           "emeld-scan-thread"))
    (emeld--update-header-line)
    (emeld--render)
    (message (if force-full
                 "Full content scan in background..."
               "Rescanning directories in background..."))))

(defun emeld-refresh-full ()
  "Force a full content-based scan (ignores `emeld-simple-comparison')."
  (interactive)
  (emeld-refresh t))

(defun emeld--read-new-root (on-left)
  "Prompt for a new root directory for side ON-LEFT and update the buffer."
  (let* ((old-root (if on-left emeld--left-root emeld--right-root))
         (side-label (if on-left "Left" "Right"))
         (new-dir (read-directory-name
                   (format "%s directory: " side-label)
                   old-root nil t)))
    (unless (string= (directory-file-name (expand-file-name new-dir))
                     (directory-file-name old-root))
      (if on-left
          (progn
            (setq emeld--left-root
                  (file-name-as-directory (expand-file-name new-dir)))
            (setq default-directory emeld--left-root)
            (emeld--apply-dir-locals))
        (setq emeld--right-root
              (file-name-as-directory (expand-file-name new-dir))))
      (setf (if on-left
                (emeld-node-left-path emeld--root-node)
              (emeld-node-right-path emeld--root-node))
            (if on-left emeld--left-root emeld--right-root))
      (setf (emeld-node-name emeld--root-node)
            (file-name-nondirectory
             (directory-file-name emeld--left-root)))
      (setf (emeld-node-name-right emeld--root-node)
            (file-name-nondirectory
             (directory-file-name emeld--right-root)))
      (emeld-refresh))))

(defun emeld-action ()
  "Perform default action on node (ediff or view).

On the header line (directory paths), prompt to replace the
current side's directory with a new one via completion.
On files, run ediff or open.  On directories, open in dired."
  (interactive)
  (let* ((node (get-text-property (point) 'emeld-node))
         (header (get-text-property (point) 'emeld-header)))
    (cond
     (header
      (let ((cur-col (current-column)))
        (emeld--read-new-root (< cur-col emeld--column-width))))
     (node
      (let* ((cur-col (current-column))
             (on-left (< cur-col emeld--column-width))
             (left (emeld-node-left-path node))
             (right (emeld-node-right-path node))
             (side-path (if on-left left right)))
        (cond ((and left right (not (emeld-node-is-dir node)))
               (if (eq (emeld-node-diff-status node) 'same)
                   (find-file-other-window side-path)
                 (ediff-files left right)))
              (side-path (find-file-other-window side-path))
              (t (message "No file on %s side"
                          (if on-left "left" "right")))))))))

(defun emeld-find-file ()
  "Open the file on the current side in another window."
  (interactive)
  (let* ((node (get-text-property (point) 'emeld-node))
         (cur-col (current-column)))
    (when node
      (let ((path (if (< cur-col emeld--column-width)
                      (emeld-node-left-path node)
                    (emeld-node-right-path node))))
        (if path
            (find-file-other-window path)
          (message "No file on %s side" (if (< cur-col emeld--column-width) "left" "right")))))))

(defun emeld-soft-diff ()
  "Contextual action for SPC.
If on a directory, toggle expansion.
If on a file, show a quick diff of the current node."
  (interactive)
  (let ((node (get-text-property (point) 'emeld-node)))
    (if (not node)
        (message "No node at point")
      (if (emeld-node-is-dir node)
          (emeld-toggle-expand)
        (let ((left (emeld-node-left-path node))
              (right (emeld-node-right-path node)))
          (if (and left right (not (file-directory-p left)) (not (file-directory-p right)))
              (diff left right)
            (message "Soft diff only works for comparing two files")))))))

(defun emeld-copy-path-at-point (arg)
  "Copy the filename at point to the kill ring.
With prefix ARG, copy the full absolute path.
The side is determined by the current column."
  (interactive "P")
  (let* ((node (get-text-property (point) 'emeld-node))
         (cur-col (current-column)))
    (if (not node)
        (message "No node at point")
      (let* ((path (if (< cur-col emeld--column-width)
                       (emeld-node-left-path node)
                     (emeld-node-right-path node)))
             (text (if arg path (emeld-node-name node))))
        (if text
            (progn
              (kill-new text)
              (message "Copied: %s" text))
          (message "No file on this side"))))))

(defun emeld-copy ()
  "Copy the file/dir at point to the other side.
Creates missing parent directories on the destination side if needed."
  (interactive)
  (let* ((node (get-text-property (point) 'emeld-node))
         (cur-col (current-column))
         (on-left (< cur-col emeld--column-width)))
    (unless node (user-error "No node at point"))
    (let* ((source (if on-left (emeld-node-left-path node) (emeld-node-right-path node)))
           (source-root (if on-left emeld--left-root emeld--right-root))
           (dest-root (if on-left emeld--right-root emeld--left-root)))
      (unless source (user-error "No file on current side to copy"))
      (let* ((rel-path (file-relative-name source source-root))
             (dest (directory-file-name (expand-file-name rel-path dest-root)))
             (dest-parent (file-name-directory dest)))
        (when (yes-or-no-p (format "Copy %s to %s?" (emeld-node-name node) dest-root))
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
                (emeld-refresh)
                (message "Copied %s" (emeld-node-name node)))
            (error (message "Copy failed: %s" (error-message-string err)))))))))

(defun emeld-delete ()
  "Delete the file/directory at point from the current side."
  (interactive)
  (let* ((node (get-text-property (point) 'emeld-node))
         (cur-col (current-column))
         (on-left (< cur-col emeld--column-width)))
    (unless node (user-error "No node at point"))
    (let* ((path (if on-left (emeld-node-left-path node) (emeld-node-right-path node)))
           (side (if on-left "left" "right")))
      (unless path (user-error "No file on %s side to delete" side))
      (when (yes-or-no-p (format "Delete %s from %s side?" (emeld-node-name node) side))
        (condition-case err
            (progn
              (if (file-directory-p path)
                  (delete-directory path t t)
                (delete-file path t))
              (emeld-refresh)
              (message "Deleted %s from %s side" (emeld-node-name node) side))
          (error (message "Delete failed: %s" (error-message-string err))))))))

(defun emeld--toggle-indicator (label enabled)
  "Return a propertized string for toggle LABEL showing ENABLED state."
  (propertize label 'face (if enabled 'bold 'shadow)))

(defun emeld--display-state-string ()
  "Return a compact string showing current display toggle states."
  (let ((groups (mapcar (lambda (g) (concat "[" g "]"))
                        emeld--active-filter-groups)))
     (concat
      (emeld--toggle-indicator "F" emeld-show-filtered)
     (emeld--toggle-indicator "s" emeld-show-same)
     (emeld--toggle-indicator "m" emeld-show-different)
     (emeld--toggle-indicator "a" emeld-show-added)
     (emeld--toggle-indicator "T" emeld-show-tree-lines)
     (when groups
       (concat " " (mapconcat #'identity groups " "))))))

(defun emeld--update-header-line ()
  "Update the header line with legend and status."
  (setq header-line-format
        '(:eval
          (let ((scan-msg (if (and (boundp 'emeld--scan-thread) 
                                    emeld--scan-thread 
                                    (thread-live-p emeld--scan-thread))
                               (propertize " [SCANNING...] " 'face 'highlight)
                             ""))
                (mode-msg (concat
                           (emeld--toggle-indicator "FAST" (eq emeld--last-scan-mode 'fast))
                           " "
                           (emeld--toggle-indicator "NORMAL" (not (eq emeld--last-scan-mode 'fast))))))
            (concat
             scan-msg mode-msg
              " [" (emeld--display-state-string) "] ")))))


(defun emeld--render ()
  "Render the tree in the current buffer, attempting to preserve point."
  (let ((inhibit-read-only t)
        (old-node (get-text-property (point) 'emeld-node))
        (old-col (current-column))
        (target-path nil))
    ;; Save path of current node to restore point later
    (when old-node
      (setq target-path (or (emeld-node-left-path old-node)
                           (emeld-node-right-path old-node))))
    
    (setq emeld--column-width (/ (* (window-width) emeld-column-ratio) 100))
    (erase-buffer)
    ;; Headers (with emeld-header property for interactive directory switching)
    (let ((header-start (point)))
      (insert (propertize (truncate-string-to-width (abbreviate-file-name emeld--left-root)
                                                    emeld--column-width nil ?\s)
                          'face 'emeld-header-face))
      (insert (propertize (truncate-string-to-width (abbreviate-file-name emeld--right-root)
                                                    emeld--column-width nil ?\s)
                          'face 'emeld-header-face))
      (add-text-properties header-start (point) '(emeld-header t mouse-face highlight)))
    (insert "\n" (make-string (* 2 emeld--column-width) ?-) "\n")
    ;; Root node (collapsible)
    (emeld--render-nodes (list emeld--root-node) "" emeld--column-width)
    
    ;; Restore point
    (if target-path
        (let ((found nil))
          (goto-char (point-min))
          (while (and (not found) (not (eobp)))
            (let ((node (get-text-property (point) 'emeld-node)))
              (if (and node (or (string= (emeld-node-left-path node) target-path)
                                (string= (emeld-node-right-path node) target-path)))
                  (progn
                    (setq found t)
                    (move-to-column old-col))
                (forward-line 1))))
          (unless found (goto-char (point-min))))
      (goto-char (point-min)))))

(defun emeld--node-visible-p (node)
  "Determine if NODE should be visible based on status filters.
The root node is always visible."
  (or (eq node emeld--root-node)
      (let ((status (emeld-node-diff-status node)))
        (cond ((eq status 'same) emeld-show-same)
              ((eq status 'different) emeld-show-different)
              ((memq status '(left-only right-only)) emeld-show-added)
              (t t)))))

(defun emeld--node-relpath (node)
  "Return the relative path of NODE from the root."
  (let ((parts nil)
        (n node))
    (while (and n (emeld-node-parent n))
      (push (emeld-node-name n) parts)
      (setq n (emeld-node-parent n)))
    (mapconcat #'identity parts "/")))

(defun emeld--collect-changed-leaves (nodes)
  "Recursively collect leaf (non-directory) NODES with changes."
  (let ((result nil))
    (dolist (node nodes)
      (if (emeld-node-is-dir node)
          (setq result (nconc result (emeld--collect-changed-leaves (emeld-node-children node))))
        (when (and (not (emeld-node-is-filtered node))
                   (memq (emeld-node-diff-status node) '(different left-only right-only)))
          (push node result))))
    (nreverse result)))

(defun emeld--write-patch-hunk (node)
  "Generate a unified-diff hunk string for a single NODE.
Patch direction is left→right: applying the patch to the right directory
makes it match the left."
  (let* ((left (emeld-node-left-path node))
         (right (emeld-node-right-path node))
         (status (emeld-node-diff-status node))
         (rel (emeld--node-relpath node))
         (patch nil))
    (with-temp-buffer
      (cond
       ((eq status 'different)
        (call-process "diff" nil t nil "-u" right left))
       ((eq status 'left-only)
        (call-process "diff" nil t nil "-u" "/dev/null" left))
       ((eq status 'right-only)
        (call-process "diff" nil t nil "-u" right "/dev/null")))
      (goto-char (point-min))
      (when (re-search-forward "^--- .*" nil t)
        (replace-match (concat "--- a/" rel)))
      (when (re-search-forward "^\\+\\+\\+ .*" nil t)
        (replace-match (concat "+++ b/" rel)))
      (buffer-string))))

(defun emeld-format-patch ()
  "Generate a unified-diff patch of all changed files and write to a file.
Prompts for an output filename, defaulting to \"emeld.patch\"."
  (interactive)
  (unless emeld--root-node
    (user-error "No comparison loaded"))
  (let* ((default-name (expand-file-name "emeld.patch" default-directory))
         (outfile (read-file-name "Write patch to: " nil default-name nil
                                  (file-name-nondirectory default-name)))
         (nodes (emeld--collect-changed-leaves (emeld-node-children emeld--root-node))))
    (if (null nodes)
        (message "No changes to write to patch")
      (with-temp-buffer
        (dolist (node nodes)
          (insert (emeld--write-patch-hunk node)))
        (write-region nil nil outfile nil 'silent)
        (message "Wrote patch with %d change(s) to %s" (length nodes) outfile)))))

(defun emeld--render-nodes (nodes indent-prefix column-width)
  "Render a list of NODES with INDENT-PREFIX and COLUMN-WIDTH."
  (let ((count (length nodes))
        (idx 0))
    (dolist (node nodes)
      (setq idx (1+ idx))
      (when (emeld--node-visible-p node)
        (let* ((status (emeld-node-diff-status node))
               (is-last (= idx count))
               (branch (if emeld-show-tree-lines
                           (concat (if is-last "└" "├")
                                   (make-string (1- emeld-indent-offset) ?─))
                         (make-string emeld-indent-offset ?\s)))
               (full-prefix (concat indent-prefix branch))
               (face (cond ((eq status 'filtered) 'emeld-filtered-face)
                           ((eq status 'same) 'emeld-same-face)
                           ((eq status 'different) 'emeld-diff-face)
                           ((eq status 'left-only) 'emeld-left-only-face)
                           ((eq status 'right-only) 'emeld-right-only-face)))
               (name-face (if (emeld-node-is-dir node) 'emeld-dir-face face))
               (dir-indicator (if (emeld-node-is-dir node)
                                    (concat (if (emeld-node-expanded node) "[-] " "[+] ")
                                            emeld-dir-prefix)
                                  "")))
           (let ((start (point)))
             ;; Left Side
             (insert (propertize full-prefix 'face 'shadow))
             (when (emeld-node-left-path node)
               (insert dir-indicator)
               (insert (propertize (emeld-node-name node) 'face name-face)))
            
            ;; Padding to Right Side
            (let ((current-len (current-column)))
              (if (< current-len column-width)
                  (insert (make-string (- column-width current-len) ?\s))
                (insert "  ")))
            
            ;; Right Side
            (insert (propertize full-prefix 'face 'shadow))
            (when (emeld-node-right-path node)
              (insert dir-indicator)
               (insert (propertize (or (emeld-node-name-right node)
                                      (emeld-node-name node))
                                   'face name-face)))
            
            (add-text-properties start (point) `(emeld-node ,node mouse-face highlight))
            (insert "\n")
            
            ;; Recurse
            (when (and (emeld-node-expanded node) (emeld-node-children node))
              (emeld--render-nodes (emeld-node-children node)
                                  (if emeld-show-tree-lines
                                      (concat indent-prefix 
                                              (if is-last " " "│")
                                              (make-string (1- emeld-indent-offset) ?\s))
                                    (concat indent-prefix (make-string emeld-indent-offset ?\s)))
                                   column-width))))))))

(transient-define-prefix emeld-dispatch ()
   "Transient menu for emeld."
     [["View"
       ("s" (lambda () (if emeld-show-same "[x] same" "[ ] same"))
        (lambda () (interactive) (emeld-toggle-show-same)) :transient t)
       ("m" (lambda () (if emeld-show-different "[x] modified" "[ ] modified"))
        (lambda () (interactive) (emeld-toggle-show-different)) :transient t)
       ("a" (lambda () (if emeld-show-added "[x] added" "[ ] added"))
        (lambda () (interactive) (emeld-toggle-show-added)) :transient t)
       ("F" (lambda () (if emeld-show-filtered "[x] filtered" "[ ] filtered"))
        (lambda () (interactive) (emeld-toggle-show-filtered)) :transient t)
       ("T" (lambda () (if emeld-show-tree-lines "[x] tree-lines" "[ ] tree-lines"))
        (lambda () (interactive) (emeld-toggle-tree-lines)) :transient t)
       (">" "indent more"  emeld-increase-indent :transient t)
       ("<" "indent less"  emeld-decrease-indent :transient t)
       ("}" "pane wider"   emeld-grow-column :transient t)
       ("{" "pane narrow"  emeld-shrink-column :transient t)]
      ["Navigate"
       ("g"   "rescan (simple)"    emeld-refresh)
       ("G"   "rescan (full diff)" emeld-refresh-full)
       ("o"   "jump other side"    emeld-jump-other-side :transient t)
       ("M-n" "next change"        emeld-next-change :transient t)
       ("M-p" "previous change"    emeld-previous-change :transient t)]
      ["File"
       ("SPC" "soft diff / fold"   emeld-soft-diff)
       ("RET" "action (ediff)"     emeld-action)
       ("C"   "copy to other side" emeld-copy)
       ("d"   "delete from side"   emeld-delete)
       ("w"   "copy path"          emeld-copy-path-at-point)
       ("P"   "format as patch"    emeld-format-patch)]
      ["Filename"
       :if (lambda () emeld-filter-groups)
       :class transient-column
       :setup-children
       (lambda (&rest _)
         (when emeld-filter-groups
            (let ((idx 0)
                  (suffixes nil)
                  (max-len (apply #'max (mapcar (lambda (g) (length (car g)))
                                                emeld-filter-groups))))
              (dolist (g emeld-filter-groups)
                 (let* ((name (car g))
                        (key (format "c%c" (+ ?a idx))))
                   (setq idx (1+ idx))
                   (push (transient-parse-suffix
                           'emeld-dispatch
                           (list key
                                 (format (concat "[%c] %-" (number-to-string max-len) "s")
                                         (if (member name emeld--active-filter-groups) ?x ?\s)
                                         name)
                                  `(lambda () (interactive) (emeld-toggle-filter-group ,name))
                                  :transient t))
                          suffixes)))
              (push (transient-parse-suffix
                     'emeld-dispatch
                      '("cl" "list all globs..." emeld-list-filter-groups))
                    suffixes)
              (nreverse suffixes))))]])

(define-key emeld-mode-map (kbd "?") #'emeld-dispatch)

(provide 'emeld)
;;; emeld.el ends here
