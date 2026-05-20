;;; mild.el --- Meld-Inspired Local Diff -*- lexical-binding: t; -*-

;; Author: Antigravity
;; Keywords: files, tools
;; Version: 0.1

;;; Commentary:
;; A simple side-by-side directory comparison tool inspired by Meld.

;;; Code:

(require 'cl-lib)
(require 'dired)

(defgroup mild nil
  "Meld-Inspired Local Diff."
  :group 'tools)

(defcustom mild-filter-globs '(".git" ".DS_Store" "*.elc" "*.pyc")
  "List of glob patterns to filter out of the comparison.
These match recursively."
  :type '(repeat string)
  :group 'mild)

(defcustom mild-show-filtered t
  "Whether to show filtered files (matching `mild-filter-globs`)."
  :type 'boolean
  :group 'mild)

(defcustom mild-show-hidden nil
  "Whether to show hidden (dot) files."
  :type 'boolean
  :group 'mild)

(cl-defstruct (mild-node
               (:constructor mild-node-create))
  name
  left-path
  right-path
  parent
  children
  (expanded t)
  (diff-status nil) ; 'same, 'different, 'left-only, 'right-only, 'filtered
  (is-dir nil)
  (is-filtered nil))

(defvar-local mild--left-root nil)
(defvar-local mild--right-root nil)
(defvar-local mild--root-node nil)

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
               mild-filter-globs)))

(defun mild--get-files (dir)
  "Get all files in DIR except . and .."
  (when (and dir (file-directory-p dir))
    (directory-files dir t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))

(defun mild--compare-files (left right)
  "Compare LEFT and RIGHT files. Return 'same or 'different."
  (if (and left right
           (file-exists-p left)
           (file-exists-p right))
      (let ((attr-left (file-attributes left))
            (attr-right (file-attributes right)))
        (if (and (eq (file-directory-p left) (file-directory-p right))
                 (or (file-directory-p left)
                     (and (eq (file-attribute-size attr-left) (file-attribute-size attr-right))
                          (time-equal-p (file-attribute-modification-time attr-left)
                                        (file-attribute-modification-time attr-right)))))
            'same
          'different))
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
      (setq mild--left-root (file-name-as-directory (expand-file-name dir1)))
      (setq mild--right-root (file-name-as-directory (expand-file-name dir2)))
      (setq mild--root-node (mild-node-create :name "ROOT"
                                             :left-path mild--left-root
                                             :right-path mild--right-root
                                             :is-dir t
                                             :expanded t))
      (setf (mild-node-children mild--root-node)
            (mild--scan-dir mild--left-root mild--right-root mild--root-node))
      (mild--render))
    (switch-to-buffer buf)))

(defface mild-same-face
  '((t :inherit font-lock-comment-face))
  "Face for files that are the same."
  :group 'mild)

(defface mild-diff-face
  '((t :foreground "red" :weight bold))
  "Face for files that are different."
  :group 'mild)

(defface mild-left-only-face
  '((t :foreground "forest green"))
  "Face for files that exist only on the left side."
  :group 'mild)

(defface mild-right-only-face
  '((t :foreground "royal blue"))
  "Face for files that exist only on the right side."
  :group 'mild)

(defface mild-header-face
  '((t :inherit font-lock-function-name-face :weight bold :underline t))
  "Face for the directory headers."
  :group 'mild)

(defface mild-filtered-face
  '((t :foreground "gray50" :strike-through t))
  "Face for filtered files."
  :group 'mild)

(defvar mild-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'mild-toggle-expand)
    (define-key map (kbd "RET") #'mild-action)
    (define-key map (kbd "g")   #'mild-refresh)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "p")   #'previous-line)
    (define-key map (kbd "C")   #'mild-copy)
    (define-key map (kbd "h")   #'mild-toggle-show-hidden)
    (define-key map (kbd "f")   #'mild-toggle-show-filtered)
    map)
  "Keymap for `mild-mode'.")

(define-derived-mode mild-mode special-mode "Mild"
  "Major mode for Meld-Inspired Local Diff."
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t))

(defun mild-toggle-show-hidden ()
  "Toggle showing hidden files."
  (interactive)
  (setq mild-show-hidden (not mild-show-hidden))
  (mild-refresh))

(defun mild-toggle-show-filtered ()
  "Toggle showing filtered files."
  (interactive)
  (setq mild-show-filtered (not mild-show-filtered))
  (mild-refresh))

(defun mild-toggle-expand ()
  "Toggle expansion of the node at point."
  (interactive)
  (let ((node (get-text-property (point) 'mild-node)))
    (when (and node (mild-node-is-dir node))
      (setf (mild-node-expanded node) (not (mild-node-expanded node)))
      (let ((pos (point)))
        (mild--render)
        (goto-char pos)))))

(defun mild-refresh ()
  "Refresh the comparison."
  (interactive)
  (message "Rescanning directories...")
  (setf (mild-node-children mild--root-node)
        (mild--scan-dir mild--left-root mild--right-root mild--root-node))
  (mild--render)
  (message "Rescanning done."))

(defun mild-action ()
  "Perform default action on node (ediff or view)."
  (interactive)
  (let ((node (get-text-property (point) 'mild-node)))
    (when node
      (let ((left (mild-node-left-path node))
            (right (mild-node-right-path node)))
        (cond ((and left right (not (mild-node-is-dir node)))
               (if (eq (mild-node-diff-status node) 'same)
                   (find-file-other-window left)
                 (ediff-files left right)))
              (left (find-file-other-window left))
              (right (find-file-other-window right)))))))

(defun mild-copy ()
  "Copy the file/dir to the other side."
  (interactive)
  (let ((node (get-text-property (point) 'mild-node)))
    (when (and node (not (eq (mild-node-diff-status node) 'same)))
      (let* ((left (mild-node-left-path node))
             (right (mild-node-right-path node))
             (parent (mild-node-parent node))
             (dest-dir (if left 
                           (mild-node-right-path parent)
                         (mild-node-left-path parent))))
        (when (and dest-dir (yes-or-no-p (format "Copy %s to %s?" 
                                                (or left right) 
                                                dest-dir)))
          (if (file-directory-p (or left right))
              (copy-directory (or left right) (expand-file-name (mild-node-name node) dest-dir))
            (copy-file (or left right) (expand-file-name (mild-node-name node) dest-dir)))
          (mild-refresh))))))

(defun mild--render ()
  "Render the tree in the current buffer."
  (let ((inhibit-read-only t)
        (column-width (/ (window-width) 2)))
    (erase-buffer)
    ;; Headers
    (insert (propertize (truncate-string-to-width mild--left-root column-width nil ?\s)
                        'face 'mild-header-face))
    (insert (propertize (truncate-string-to-width mild--right-root column-width nil ?\s)
                        'face 'mild-header-face))
    (insert "\n" (make-string (* 2 column-width) ?-) "\n")
    ;; Root children
    (mild--render-nodes (mild-node-children mild--root-node) 0 column-width)
    (goto-char (point-min))))

(defun mild--render-nodes (nodes indent column-width)
  "Render a list of NODES with INDENT and COLUMN-WIDTH."
  (dolist (node nodes)
    (let* ((status (mild-node-diff-status node))
           (face (cond ((eq status 'filtered) 'mild-filtered-face)
                       ((eq status 'same) 'mild-same-face)
                       ((eq status 'different) 'mild-diff-face)
                       ((eq status 'left-only) 'mild-left-only-face)
                       ((eq status 'right-only) 'mild-right-only-face)))
           (prefix (if (mild-node-is-dir node)
                       (if (mild-node-expanded node) "[-] " "[+] ")
                     "    "))
           (name (mild-node-name node))
           (indent-str (make-string (* indent 2) ?\s))
           (start (point)))
      
      ;; Left Column
      (insert indent-str prefix)
      (if (mild-node-left-path node)
          (insert (propertize name 'face face))
        (insert (make-string (length name) ?\s)))
      (let ((current-col (- (point) (line-beginning-position))))
        (when (< current-col column-width)
          (insert (make-string (- column-width current-col) ?\s))))
      
      ;; Right Column
      (insert indent-str prefix)
      (if (mild-node-right-path node)
          (insert (propertize name 'face face))
        (insert (make-string (length name) ?\s)))
      
      (add-text-properties start (point) `(mild-node ,node mouse-face highlight))
      (insert "\n")
      
      (when (and (mild-node-is-dir node) (mild-node-expanded node))
        (mild--render-nodes (mild-node-children node) (+ indent 1) column-width)))))

(provide 'mild)
;;; mild.el ends here
