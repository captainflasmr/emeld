;;; emeld.el --- Meld-Inspired Local Diff -*- lexical-binding: t; -*-

;; Keywords: files, tools
;; Version: 0.11.0

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

(defcustom emeld-show-strikethrough t
  "Whether to show strikethrough for orphan files on the missing side.
When non-nil, a file that exists only on one side shows its tree path
and filename with strikethrough on the other side, mimicking Meld's
display.  When nil, the other side shows nothing for the orphan."
  :type 'boolean
  :group 'emeld)

(defcustom emeld-column-ratio 50
  "Width of the left column as a percentage of the window width (10-90).
Defaults to 50 for equal-width panes."
  :type 'integer
  :group 'emeld)

(defcustom emeld-max-content-size (* 2 1024 1024)
  "Maximum file size (bytes) to read for content comparison.
Larger files fall back to size-only comparison in the pure-Elisp path.
Has no effect when `file-equal-p' is available (Emacs >= 29)."
  :type 'integer
  :group 'emeld)

(defvar emeld-simple-comparison nil
  "Obsolete.  Removed in 0.9.4.  Kept for dir-locals compatibility.")
(make-obsolete-variable 'emeld-simple-comparison nil "0.9.4")

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
  (diff-status nil) ; 'same, 'different, 'left-only, 'right-only
  (is-dir nil)
  (is-filtered nil))

(defvar-local emeld--scan-process nil
  "Currently running `make-process' for the async diff scan.")
(defvar-local emeld--scan-diff-lines nil
  "Diff status lines from `diff -rq' output.")
(defvar-local emeld--scan-buf nil
  "Buffer being scanned (for sentinel callbacks).")
(defvar-local emeld--cached-full-lines nil
  "Non-nil when a `-s' scan has completed (identical files available in tree).")
(defvar-local emeld--active-filter-groups nil
  "List of currently active filter group names.")
(defvar-local emeld--left-root nil)
(defvar-local emeld--right-root nil)
(defvar-local emeld--root-node nil)
(defvar-local emeld--column-width nil)
(defvar-local emeld--saved-expanded nil
  "Saved expanded directory paths to restore after refresh.")

;;;; ─── Async scan via make-process ─────────────────────────────────

(defun emeld--sentinel-done-p (event)
  "Return non-nil if EVENT indicates a process has finished (any exit code)."
  (or (string-prefix-p "finished" event)
      (string-prefix-p "exited" event)))

(defun emeld--strip-dir-prefix (line dir)
  "Strip the absolute DIR prefix from LINE, leaving a relative path."
  (let ((prefix (file-name-as-directory (expand-file-name dir))))
    (if (string-prefix-p prefix line)
        (substring line (length prefix))
      line)))

(defun emeld--make-file-node (relpath parent status)
  "Create emeld-node for a file at RELPATH (relative from root).
Only sets the path for the side(s) where the file exists."
  (let ((name (file-name-nondirectory relpath)))
    (emeld-node-create
     :name name
     :left-path (and (memq status '(same different left-only))
                     (expand-file-name relpath emeld--left-root))
     :right-path (and (memq status '(same different right-only))
                      (expand-file-name relpath emeld--right-root))
     :parent parent
     :diff-status status)))

(defun emeld--entries-to-nodes (entries prefix parent)
  "Convert ENTRIES (alist of (REL-PATH . STATUS)) into emeld-node tree.
Directories are grouped first, followed by files, each sorted alphabetically."
  (let ((dirs (make-hash-table :test 'equal))
        (files nil))
    (dolist (entry entries)
      (let* ((path (car entry))
             (status (cdr entry))
             (name (file-name-nondirectory path))
             (filt (emeld--filtered-p name path)))
        (unless filt
          (if (string-match "/" path)
              (let* ((slash-pos (match-beginning 0))
                     (dir-name (substring path 0 slash-pos))
                     (rest (substring path (1+ slash-pos))))
                (push (cons rest status)
                      (gethash dir-name dirs)))
            (push (cons path status) files)))))
    (let ((dir-names (make-hash-table :test 'equal)))
      (maphash (lambda (k _) (puthash k t dir-names)) dirs)
      (let ((file-nodes
             (delq nil
                   (mapcar (lambda (entry)
                             (let ((path (car entry))
                                   (status (cdr entry)))
                               (if (gethash path dir-names)
                                   nil
                                 (let ((sub-rel (if (string-empty-p prefix) path
                                                  (concat prefix "/" path))))
                                   (emeld--make-file-node sub-rel parent status)))))
                           (sort files (lambda (a b) (string< (car a) (car b)))))))
            (dir-nodes nil))
        (maphash
         (lambda (dir-name sub-entries)
           (let ((sub-prefix (if (string-empty-p prefix) dir-name
                               (concat prefix "/" dir-name))))
             (unless (emeld--filtered-p dir-name sub-prefix)
               (let* ((dir-node (emeld-node-create
                                 :name dir-name
                                 :left-path (expand-file-name sub-prefix emeld--left-root)
                                 :right-path (expand-file-name sub-prefix emeld--right-root)
                                 :parent parent
                                 :is-dir t
                                 :diff-status 'same))
                      (sub-nodes (emeld--entries-to-nodes
                                  sub-entries sub-prefix dir-node))
                      (has-changes (cl-some (lambda (n)
                                              (not (eq (emeld-node-diff-status n) 'same)))
                                            sub-nodes)))
                 (when has-changes
                   (setf (emeld-node-diff-status dir-node) 'different))
                 (setf (emeld-node-children dir-node) sub-nodes)
                 (setq dir-nodes (cons dir-node dir-nodes))))))
         dirs)
        ;; MODIFIED HERE: Sort directories first, then files.
        (sort (nconc file-nodes (nreverse dir-nodes))
              (lambda (a b)
                (let ((a-dir (emeld-node-is-dir a))
                      (b-dir (emeld-node-is-dir b)))
                  (if (eq a-dir b-dir)
                      ;; If both are dirs or both are files, sort alphabetically
                      (string< (emeld-node-name a) (emeld-node-name b))
                    ;; Otherwise, the one that is a directory comes first
                    a-dir))))))))

(defun emeld--expand-dir-entries (entries)
  "Expand left-only/right-only entries that are directories on disk.
`diff -rq' only reports the directory name, not its contents, for
directories that exist entirely on one side."
  (let ((result nil))
    (dolist (entry entries)
      (let ((path (car entry))
            (status (cdr entry)))
        (cond
         ((and (eq status 'left-only)
               (file-directory-p (expand-file-name path emeld--left-root)))
          (dolist (f (directory-files-recursively
                      (expand-file-name path emeld--left-root) ".*"))
            (push (cons (file-relative-name f emeld--left-root) 'left-only)
                  result)))
         ((and (eq status 'right-only)
               (file-directory-p (expand-file-name path emeld--right-root)))
          (dolist (f (directory-files-recursively
                      (expand-file-name path emeld--right-root) ".*"))
            (push (cons (file-relative-name f emeld--right-root) 'right-only)
                  result)))
         (t
          (push entry result)))))
    (nreverse result)))

(defun emeld--build-tree-from-diff (buf)
  "Build tree from `diff -rq' output only, then render."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((left emeld--left-root)
            (right emeld--right-root)
            (root emeld--root-node)
            (gc-cons-threshold most-positive-fixnum)
            (entries nil))
        (dolist (line emeld--scan-diff-lines)
          (let ((parsed (emeld--parse-diff-line line left right)))
            (when parsed
              (push parsed entries))))
        (when (cl-some (lambda (e) (memq (cdr e) '(left-only right-only))) entries)
          (setq entries (emeld--expand-dir-entries entries)))
        (setf (emeld-node-children root)
              (emeld--entries-to-nodes entries "" root))
        (emeld--restore-expanded root)
        (setf (emeld-node-diff-status root)
              (if (cl-some (lambda (c) (not (eq (emeld-node-diff-status c) 'same)))
                           (emeld-node-children root))
                  'different 'same))
        (setq emeld--scan-process nil
              emeld--scan-diff-lines nil
              emeld--scan-buf nil)
        (when emeld-show-same
          (setq emeld--cached-full-lines t))
        (emeld--update-header-line)
        (emeld--render)
        (message "Scanning done.")))))

(defun emeld--diff-exclude-args ()
  "Return `--exclude=PATTERN' args from active filter groups.
Only includes patterns without `/', since `diff --exclude' matches
the basename component, not the full relative path."
  (let ((args '("--exclude=node_modules")))
    (dolist (group emeld-filter-groups)
      (when (member (car group) emeld--active-filter-groups)
        (dolist (glob (emeld--group-globs group))
          (unless (string-match-p "/" glob)
            (push (concat "--exclude=" glob) args)))))
    args))

(defun emeld--start-diff (buf)
  "Start async `diff -rq' for content comparison.
Active filter group patterns (without `/') are passed to `diff'
as `--exclude' arguments so diff skips filtered files entirely."
  (setq emeld--scan-diff-lines nil)
  (let* ((excludes (emeld--diff-exclude-args))
         (flags (if emeld-show-same "-rqs" "-rq"))
         (command `("diff" ,flags
                    ,@excludes
                    ,(expand-file-name emeld--left-root)
                    ,(expand-file-name emeld--right-root)))
         (proc (make-process
                :name "emeld-diff"
                :buffer (generate-new-buffer " *emeld-diff*")
                :command command
                :sentinel
                (lambda (proc event)
                  (when (emeld--sentinel-done-p event)
                    (when (buffer-live-p buf)
                      (with-current-buffer buf
                        (let ((raw (with-current-buffer (process-buffer proc)
                                     (buffer-string))))
                          (setq emeld--scan-diff-lines
                                (split-string raw "[\n\r]+" t))
                          (kill-buffer (process-buffer proc))
                          (emeld--build-tree-from-diff buf)))))))))
    (setq emeld--scan-process proc)))

(defun emeld--parse-diff-line (line left-dir right-dir)
  "Parse a `diff -rq' output LINE into (REL-PATH . STATUS) or nil."
  (cond
   ((string-match "^Files \\(.+\\) and .+ differ$" line)
    (let ((rel (emeld--strip-dir-prefix (match-string 1 line) left-dir)))
      (cons rel 'different)))
   ((string-match "^Files \\(.+\\) and .+ are identical$" line)
    (let ((rel (emeld--strip-dir-prefix (match-string 1 line) left-dir)))
      (cons rel 'same)))
   ((string-match "^Only in \\(.+\\): \\(.+\\)$" line)
    (let* ((dir-part (match-string 1 line))
           (name (match-string 2 line))
           (full-path (concat (file-name-as-directory dir-part) name))
           (left-prefix (expand-file-name left-dir))
           (rel (if (string-prefix-p left-prefix full-path)
                    (emeld--strip-dir-prefix full-path left-dir)
                  (emeld--strip-dir-prefix full-path right-dir)))
           (side (if (string-prefix-p left-prefix full-path)
                     'left-only 'right-only)))
      (cons rel side)))
   (t nil)))

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

(defun emeld--files-equal-p (file1 file2)
  "Return t if FILE1 and FILE2 have identical contents.
Compares byte-by-byte using pure Elisp.
Files larger than `emeld-max-content-size' are compared by size only."
  (let* ((attrs1 (file-attributes file1))
         (attrs2 (file-attributes file2))
         (size1 (and attrs1 (file-attribute-size attrs1)))
         (size2 (and attrs2 (file-attribute-size attrs2))))
    (and size1 size2 (= size1 size2)
         (if (> size1 emeld-max-content-size)
             t
           (with-temp-buffer
             (insert-file-contents-literally file1)
             (let ((buf1-string (buffer-string)))
               (erase-buffer)
               (insert-file-contents-literally file2)
               (string= buf1-string (buffer-string))))))))

(defun emeld--compare-files (left right)
  "Compare LEFT and RIGHT files. Return \\='same or \\='different."
  (if (and left right
           (file-exists-p left)
           (file-exists-p right))
      (let ((is-dir-left (file-directory-p left))
            (is-dir-right (file-directory-p right)))
        (cond ((not (eq is-dir-left is-dir-right)) 'different)
              (is-dir-left 'same)
              (t (if (emeld--files-equal-p left right)
                     'same 'different))))
    (cond (left 'left-only)
          (right 'right-only)
          (t 'same))))

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

(defface emeld-left-only-strike-face
  '((t :inherit emeld-left-only-face :strike-through t))
  "Face for left-only files rendered on the right side with strikethrough.
Mimics Meld's display of missing files."
  :group 'emeld)

(defface emeld-right-only-strike-face
  '((t :inherit emeld-right-only-face :strike-through t))
  "Face for right-only files rendered on the left side with strikethrough.
Mimics Meld's display of missing files."
  :group 'emeld)

(defface emeld-header-face
  '((t :inherit font-lock-function-name-face :weight bold :underline t))
  "Face for the directory headers."
  :group 'emeld)

(defface emeld-dir-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for directory names (overrides diff-status faces)."
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
    (define-key map (kbd "s")   #'emeld-toggle-show-same)
    (define-key map (kbd "m")   #'emeld-toggle-show-different)
    (define-key map (kbd "a")   #'emeld-toggle-show-added)
    (define-key map (kbd "T")   #'emeld-toggle-tree-lines)
    (define-key map (kbd "S")   #'emeld-toggle-show-strikethrough)
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
  (make-local-variable 'emeld-show-same)
  (make-local-variable 'emeld-show-different)
  (make-local-variable 'emeld-show-added)
  (make-local-variable 'emeld-show-strikethrough)
  (setq-local revert-buffer-function #'emeld-refresh)
  (add-hook 'kill-buffer-hook #'emeld--stop-scan nil t)
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
      (when (and node (emeld-node-parent node)) ;; Only snap for children, not root nodes
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

(defun emeld-toggle-show-same ()
  "Toggle showing identical files.
Uses cached data when available to avoid re-scanning."
  (interactive)
  (let ((new-val (not emeld-show-same)))
    (when (and new-val (not emeld--cached-full-lines)
               (not (y-or-n-p "Show identical files?  This can take a while and may temporarily freeze Emacs.  Proceed? ")))
      (message "Show same: off")
      (setq new-val nil))
    (setq emeld-show-same new-val)
    (message "Show same: %s" (if emeld-show-same "on" "off"))
    (if (and emeld-show-same (not emeld--cached-full-lines))
        (emeld-refresh)
      (emeld--render))))

(defun emeld-toggle-tree-lines ()
  "Toggle the display of tree lines."
  (interactive)
  (setq emeld-show-tree-lines (not emeld-show-tree-lines))
  (emeld--render))

(defun emeld-toggle-show-strikethrough ()
  "Toggle strikethrough display for orphan files on the missing side.
When enabled, orphan files are shown with strikethrough on the side
where they are missing (Meld-style).  When disabled, the missing side
shows nothing for the orphan."
  (interactive)
  (setq emeld-show-strikethrough (not emeld-show-strikethrough))
  (message "Strikethrough: %s" (if emeld-show-strikethrough "on" "off"))
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
  "Cancel the active async scan and clean up processes."
  (when (and emeld--scan-process
             (process-live-p emeld--scan-process))
    (delete-process emeld--scan-process))
  (setq emeld--scan-process nil
        emeld--scan-diff-lines nil
        emeld--scan-buf nil))

(defun emeld-refresh ()
  "Refresh the comparison asynchronously.
Uses `make-process' to run `diff -rq' as an OS-level async process,
so Emacs stays fully responsive during the scan."
  (interactive)
  (emeld--stop-scan)
  (let* ((buf (current-buffer))
         (root emeld--root-node))
    (setq emeld--scan-buf buf
          emeld--saved-expanded (emeld--collect-expanded root)
          emeld--cached-full-lines nil)
    (setf (emeld-node-children root) nil)
    (emeld--update-header-line)
    (emeld--render)
    (emeld--start-diff buf)
    (message "Scanning in background...")))

(defun emeld--collect-expanded (node)
  "Return a list of relative paths for expanded directories under NODE."
  (let ((paths nil))
    (when (emeld-node-children node)
      (dolist (child (emeld-node-children node))
        (when (emeld-node-is-dir child)
          (let ((rel (emeld--node-relpath child)))
            (when (emeld-node-expanded child)
              (push rel paths))
            (setq paths (nconc (emeld--collect-expanded child) paths))))))
    paths))

(defun emeld--restore-expanded (node)
  "Restore expansion state from `emeld--saved-expanded' for tree under NODE."
  (when emeld--saved-expanded
    (dolist (child (emeld-node-children node))
      (when (emeld-node-is-dir child)
        (let ((rel (emeld--node-relpath child)))
          (unless (member rel emeld--saved-expanded)
            (setf (emeld-node-expanded child) nil))
          (emeld--restore-expanded child))))))

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
                    (if (fboundp 'copy-directory-contents)
                        (copy-directory source dest t t t)
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
     (emeld--toggle-indicator "s" emeld-show-same)
     (emeld--toggle-indicator "m" emeld-show-different)
     (emeld--toggle-indicator "a" emeld-show-added)
     (emeld--toggle-indicator "T" emeld-show-tree-lines)
     (emeld--toggle-indicator "S" emeld-show-strikethrough)
     (when groups
       (concat " " (mapconcat #'identity groups " "))))))

(defun emeld--update-header-line ()
  "Update the header line with legend and status."
  (setq header-line-format
        '(:eval
          (let ((scan-msg (if (and (boundp 'emeld--scan-process)
                                   emeld--scan-process)
                              (propertize " [SCANNING...] " 'face 'highlight)
                            "")))
            (concat
             scan-msg
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
         (rel (emeld--node-relpath node)))
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
               (face (cond ((eq status 'same) 'emeld-same-face)
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
            (if (emeld-node-left-path node)
                (progn
                  (insert dir-indicator)
                  (insert (propertize (emeld-node-name node) 'face name-face)))
              (when (and emeld-show-strikethrough (eq status 'right-only))
                (insert dir-indicator)
                (insert (propertize (emeld-node-name node)
                                    'face 'emeld-right-only-strike-face))))

            ;; Padding to Right Side
            (let ((current-len (current-column)))
              (if (< current-len column-width)
                  (insert (make-string (- column-width current-len) ?\s))
                (insert "  ")))

            ;; Right Side
            (insert (propertize full-prefix 'face 'shadow))
            (if (emeld-node-right-path node)
                (progn
                  (insert dir-indicator)
                  (insert (propertize (or (emeld-node-name-right node)
                                          (emeld-node-name node))
                                      'face name-face)))
              (when (and emeld-show-strikethrough (eq status 'left-only))
                (insert dir-indicator)
                (insert (propertize (emeld-node-name node)
                                    'face 'emeld-left-only-strike-face))))
            
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

(defclass emeld-dynamic-suffix (transient-suffix)
  ((dynamic-description :initarg :dynamic-description :initform nil))
  "A transient suffix that evaluates a lambda for its description.")

(cl-defmethod transient-format-description ((obj emeld-dynamic-suffix))
  "Evaluate the dynamic-description slot for the display string."
  (let ((fn (oref obj dynamic-description)))
    (if fn (funcall fn) (cl-call-next-method))))

(transient-define-prefix emeld-dispatch ()
  "Transient menu for emeld."
  [["File Status"
    ("s" (lambda () (if emeld-show-same "[x] Same" "[ ] Same"))
     (lambda () (interactive) (emeld-toggle-show-same)) :transient t)
    ("a" (lambda () (if emeld-show-added "[x] New" "[ ] New"))
     (lambda () (interactive) (emeld-toggle-show-added)) :transient t)
    ("m" (lambda () (if emeld-show-different "[x] Modified" "[ ] Modified"))
     (lambda () (interactive) (emeld-toggle-show-different)) :transient t)]
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
            (let* ((group-name (car g))
                   (key (format "c%c" (+ ?a idx)))
                   (cmd (let ((g-name group-name))
                          (lambda ()
                            (interactive)
                            (emeld-toggle-filter-group g-name))))
                   (desc-func (let ((g-name group-name) (len max-len))
                                (lambda ()
                                  (format (concat "[%c] %-" (number-to-string len) "s")
                                          (if (member g-name emeld--active-filter-groups) ?x ?\s)
                                          g-name)))))
              (setq idx (1+ idx))
              (push (transient-parse-suffix
                     'emeld-dispatch
                     (list key "" cmd
                           :class 'emeld-dynamic-suffix
                           :dynamic-description desc-func
                           :transient t))
                    suffixes)))
          (push (transient-parse-suffix
                 'emeld-dispatch
                 (list "cl" "list all globs..." 'emeld-list-filter-groups))
                suffixes)
          (nreverse suffixes))))]
   ["View"
    ("T" (lambda () (if emeld-show-tree-lines "[x] tree-lines" "[ ] tree-lines"))
     (lambda () (interactive) (emeld-toggle-tree-lines)) :transient t)
    ("S" (lambda () (if emeld-show-strikethrough "[x] strikethrough" "[ ] strikethrough"))
     (lambda () (interactive) (emeld-toggle-show-strikethrough)) :transient t)
    (">" "indent more"  emeld-increase-indent :transient t)
    ("<" "indent less"  emeld-decrease-indent :transient t)
    ("}" "pane wider"   emeld-grow-column :transient t)
    ("{" "pane narrow"  emeld-shrink-column :transient t)]
   ["Navigate"
    ("g"   "rescan"             emeld-refresh)
    ("o"   "jump other side"    emeld-jump-other-side :transient t)
    ("M-n" "next change"        emeld-next-change :transient t)
    ("M-p" "previous change"    emeld-previous-change :transient t)]
   ["Action"
    ("SPC" "soft diff / fold"   emeld-soft-diff)
    ("RET" "action (ediff)"     emeld-action)
    ("C"   "copy to other side" emeld-copy)
    ("d"   "delete from side"   emeld-delete)
    ("w"   "copy path"          emeld-copy-path-at-point)
    ("P"   "format as patch"    emeld-format-patch)]
   ])

(define-key emeld-mode-map (kbd "?") #'emeld-dispatch)

(provide 'emeld)
;;; emeld.el ends here
