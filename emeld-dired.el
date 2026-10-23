;;; emeld-dired.el --- Tree-navigable Dired for emeld -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Dyer

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; A treemacs-style, navigable *tree* rendering of a single directory that
;; behaves like Dired: you can mark files, flag them for deletion, copy,
;; rename, delete and create directories, but the directory is shown as a
;; foldable tree rather than one flat listing.  Built on the same `emeld-node'
;; model and rendering helpers as the comparison view and the sidebar.
;;
;; Entry points:
;;   `M-x emeld-dired'       -- tree for the current Dired buffer / directory.
;;   `M-x emeld-dired-find'  -- tree for a prompted directory.
;;
;; Unlike `emeld-sidebar' (a docked side window that follows the active file),
;; `emeld-dired' takes over its window like Dired and is mark/operation
;; oriented.  See `emeld-core.el' for the shared core.

;;; Code:

(require 'cl-lib)
(require 'emeld-core)

;;;; ─── Customization ───────────────────────────────────────────────

(defcustom emeld-dired-show-hidden t
  "When non-nil, show dotfiles in the emeld-dired tree.
Toggle per-buffer with \\<emeld-dired-mode-map>\\[emeld-dired-toggle-hidden]."
  :type 'boolean
  :group 'emeld)

(defface emeld-dired-mark-face
  '((t :inherit warning))
  "Face for the mark glyph of a marked file in an emeld-dired tree."
  :group 'emeld)

(defface emeld-dired-flag-face
  '((t :inherit error))
  "Face for the deletion-flag glyph in an emeld-dired tree."
  :group 'emeld)

(defconst emeld--dired-mark-char ?*
  "Glyph used for an ordinary mark (\\[emeld-dired-mark]).")

(defconst emeld--dired-flag-char ?D
  "Glyph used to flag a file for deletion (\\[emeld-dired-flag-delete]).")

;;;; ─── Buffer-local state ──────────────────────────────────────────

(defvar-local emeld--dired-root nil
  "Absolute root directory (with trailing slash) of this emeld-dired tree.")

(defvar-local emeld--dired-marks nil
  "Hash table mapping an absolute path to its mark char (`*' or `D').")

;;;; ─── Tree building (lazy, one directory level at a time) ─────────

(defun emeld--dired-visible-p (name)
  "Return non-nil when child basename NAME should be shown.
Hides dotfiles unless `emeld-dired-show-hidden'."
  (and (not (member name '("." "..")))
       (or emeld-dired-show-hidden (not (string-prefix-p "." name)))))

(defun emeld--dired-dir-children (node)
  "Build NODE's immediate child nodes from disk: directories first, then files."
  (let ((dirs nil) (files nil))
    (dolist (abs (ignore-errors
                   (directory-files (emeld-node-left-path node) t
                                    directory-files-no-dot-files-regexp)))
      (let ((name (file-name-nondirectory abs)))
        (when (emeld--dired-visible-p name)
          (let ((child (emeld-node-create
                        :name name :left-path abs
                        :is-dir (file-directory-p abs)
                        :expanded nil :scanned nil :parent node)))
            (if (emeld-node-is-dir child) (push child dirs) (push child files))))))
    (cl-flet ((by-name (a b) (string-lessp (emeld-node-name a) (emeld-node-name b))))
      (append (sort dirs #'by-name) (sort files #'by-name)))))

(defun emeld--dired-populate (node)
  "Load NODE's children from disk when not already loaded (NODE's `scanned' slot)."
  (when (and (emeld-node-is-dir node) (not (emeld-node-scanned node)))
    (setf (emeld-node-children node) (emeld--dired-dir-children node)
          (emeld-node-scanned node) t)))

(defun emeld--dired-build ()
  "Build the root node and its first level of children."
  (let ((root (emeld-node-create
               :name (directory-file-name emeld--dired-root)
               :left-path emeld--dired-root
               :is-dir t :expanded t :scanned nil)))
    (emeld--dired-populate root)
    (setq emeld--root-node root)))

;;;; ─── Rendering ───────────────────────────────────────────────────

(defun emeld--dired-mark-of (path)
  "Return the mark char stored for absolute PATH, or nil."
  (and emeld--dired-marks (gethash (expand-file-name path) emeld--dired-marks)))

(defun emeld--dired-render-nodes (nodes prefix)
  "Render NODES under tree-line PREFIX into the emeld-dired buffer."
  (let ((count (length nodes)) (idx 0))
    (dolist (node nodes)
      (setq idx (1+ idx))
      (let* ((is-last (= idx count))
             (is-dir (emeld-node-is-dir node))
             (branch (emeld--tree-branch is-last))
             (mark (emeld--dired-mark-of (emeld-node-left-path node)))
             (mark-face (cond ((eq mark emeld--dired-flag-char) 'emeld-dired-flag-face)
                              (mark 'emeld-dired-mark-face)))
             (indicator (if is-dir
                            (concat (if (emeld-node-expanded node)
                                        emeld-fold-expanded-indicator
                                      emeld-fold-collapsed-indicator)
                                    emeld-dir-prefix)
                          ""))
             (face (if is-dir 'emeld-dir-face 'default))
             (start (point)))
        ;; Dired-style mark column: one glyph then a space.
        (insert (propertize (string (or mark ?\s)) 'face mark-face
                            'emeld-mark-cell t)
                " ")
        (insert (propertize (concat prefix branch) 'face 'shadow))
        (insert indicator)
        (insert (propertize (emeld-node-name node) 'face face))
        (add-text-properties start (point) `(emeld-node ,node mouse-face highlight))
        (insert "\n")
        (when (and is-dir (emeld-node-expanded node) (emeld-node-children node))
          (emeld--dired-render-nodes
           (emeld-node-children node)
           (concat prefix (emeld--tree-continuation is-last))))))))

(defun emeld--dired-render ()
  "Render the emeld-dired tree, preserving the path at point."
  (let ((inhibit-read-only t)
        (point-path (emeld--dired-path-at-point))
        (n-marked (emeld--dired-count emeld--dired-mark-char))
        (n-flagged (emeld--dired-count emeld--dired-flag-char)))
    (erase-buffer)
    (insert (propertize (abbreviate-file-name emeld--dired-root)
                        'face 'emeld-header-face)
            (cond ((> (+ n-marked n-flagged) 0)
                   (propertize (format "  (%d marked, %d flagged)" n-marked n-flagged)
                               'face 'shadow))
                  (t ""))
            "\n")
    (when emeld--root-node
      (emeld--dired-render-nodes (emeld-node-children emeld--root-node) ""))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (if (and point-path (emeld--dired-goto-path point-path))
        nil
      (forward-line 1))))

;;;; ─── Navigation helpers ─────────────────────────────────────────

(defun emeld--dired-path-at-point ()
  "Return the absolute path of the node at point, or nil."
  (let ((node (get-text-property (point) 'emeld-node)))
    (and node (emeld-node-left-path node))))

(defun emeld--dired-node-at-point ()
  "Return the `emeld-node' at point, or signal a `user-error'."
  (or (get-text-property (point) 'emeld-node)
      (user-error "No file on this line")))

(defun emeld--dired-goto-path (path)
  "Move point to the line of the node whose path is PATH; non-nil if found."
  (let ((target (expand-file-name path)) (found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found) (not (eobp)))
        (let ((node (get-text-property (point) 'emeld-node)))
          (when (and node
                     (string= (expand-file-name (emeld-node-left-path node)) target))
            (setq found (line-beginning-position))))
        (forward-line 1)))
    (when found (goto-char found) (back-to-indentation) t)))

(defun emeld--dired-node-for (file &optional expand-target)
  "Descend the tree to FILE, populating and expanding every ancestor directory.
Return the node for FILE, or nil when it is outside the root or missing.
When EXPAND-TARGET is non-nil and the target is a directory, expand it too."
  (when (and emeld--root-node emeld--dired-root)
    (let ((file (expand-file-name file)))
      (when (string-prefix-p emeld--dired-root file)
        (let* ((rel (substring file (length emeld--dired-root)))
               (parts (split-string rel "/" t))
               (node emeld--root-node))
          (setf (emeld-node-expanded node) t)
          (catch 'done
            (dolist (part parts)
              (emeld--dired-populate node)
              (setf (emeld-node-expanded node) t)
              (let ((child (cl-find-if
                            (lambda (c) (string= (emeld-node-name c) part))
                            (emeld-node-children node))))
                (unless child (throw 'done nil))
                (setq node child)))
            (when (and expand-target (emeld-node-is-dir node))
              (emeld--dired-populate node)
              (setf (emeld-node-expanded node) t))
            node))))))

(defun emeld--dired-expanded-paths (node)
  "Return the absolute paths of every expanded directory under NODE."
  (let (acc)
    (cl-labels ((walk (n)
                  (when (and (emeld-node-is-dir n) (emeld-node-expanded n))
                    (push (expand-file-name (emeld-node-left-path n)) acc)
                    (dolist (c (emeld-node-children n)) (walk c)))))
      (when node (walk node)))
    acc))

;;;; ─── Marks ───────────────────────────────────────────────────────

(defun emeld--dired-count (char)
  "Return how many paths currently carry mark CHAR."
  (let ((n 0))
    (when emeld--dired-marks
      (maphash (lambda (_ v) (when (eq v char) (cl-incf n))) emeld--dired-marks))
    n))

(defun emeld--dired-set-mark (path char)
  "Set or clear (when CHAR is nil) the mark on absolute PATH."
  (let ((key (expand-file-name path)))
    (if char (puthash key char emeld--dired-marks)
      (remhash key emeld--dired-marks))))

(defun emeld--dired-marked-paths (char)
  "Return the list of absolute paths carrying mark CHAR."
  (let (acc)
    (when emeld--dired-marks
      (maphash (lambda (k v) (when (eq v char) (push k acc))) emeld--dired-marks))
    (nreverse acc)))

(defun emeld--dired-targets ()
  "Return the operation targets: the marked files, else the file at point.
Dired semantics: ordinary marks win; otherwise the node under point."
  (or (emeld--dired-marked-paths emeld--dired-mark-char)
      (list (emeld-node-left-path (emeld--dired-node-at-point)))))

(defun emeld--dired-rerender-keep-point ()
  "Re-render while keeping point on the same line's path."
  (let ((p (emeld--dired-path-at-point)))
    (emeld--dired-render)
    (when p (emeld--dired-goto-path p))))

(defun emeld--dired-mark-line (char)
  "Put mark CHAR on the file at point and advance to the next line."
  (let ((node (emeld--dired-node-at-point)))
    (emeld--dired-set-mark (emeld-node-left-path node) char)
    (emeld--dired-render)
    (emeld--dired-goto-path (emeld-node-left-path node))
    (emeld-dired-next-line)))

(defun emeld-dired-mark ()
  "Mark the file at point with `*' and move to the next line."
  (interactive)
  (emeld--dired-mark-line emeld--dired-mark-char))

(defun emeld-dired-flag-delete ()
  "Flag the file at point for deletion (`D') and move to the next line."
  (interactive)
  (emeld--dired-mark-line emeld--dired-flag-char))

(defun emeld-dired-unmark ()
  "Remove any mark from the file at point and move to the next line."
  (interactive)
  (emeld--dired-mark-line nil))

(defun emeld-dired-unmark-backward ()
  "Move to the previous line and remove any mark there."
  (interactive)
  (emeld-dired-previous-line)
  (when (get-text-property (point) 'emeld-node)
    (let ((node (emeld--dired-node-at-point)))
      (emeld--dired-set-mark (emeld-node-left-path node) nil)
      (emeld--dired-render)
      (emeld--dired-goto-path (emeld-node-left-path node)))))

(defun emeld-dired-unmark-all ()
  "Remove every mark and deletion flag in the buffer."
  (interactive)
  (clrhash emeld--dired-marks)
  (emeld--dired-rerender-keep-point)
  (message "Removed all marks"))

(defun emeld-dired-toggle-marks ()
  "Toggle ordinary `*' marks on every visible file (flags are left alone)."
  (interactive)
  (let (visible)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((node (get-text-property (point) 'emeld-node)))
          (when (and node (not (emeld-node-is-dir node)))
            (push (emeld-node-left-path node) visible)))
        (forward-line 1)))
    (dolist (path visible)
      (let ((m (emeld--dired-mark-of path)))
        (cond ((eq m emeld--dired-mark-char) (emeld--dired-set-mark path nil))
              ((null m) (emeld--dired-set-mark path emeld--dired-mark-char))))))
  (emeld--dired-rerender-keep-point))

(defun emeld--dired-prune-missing-marks ()
  "Drop marks for paths that no longer exist on disk."
  (when emeld--dired-marks
    (let (dead)
      (maphash (lambda (k _) (unless (file-exists-p k) (push k dead)))
               emeld--dired-marks)
      (dolist (k dead) (remhash k emeld--dired-marks)))))

;;;; ─── Visiting / expansion ───────────────────────────────────────

(defun emeld-dired-toggle-node ()
  "Toggle the directory at point; on a file, visit it."
  (interactive)
  (let ((node (emeld--dired-node-at-point)))
    (if (emeld-node-is-dir node)
        (progn
          (emeld--dired-populate node)
          (setf (emeld-node-expanded node) (not (emeld-node-expanded node)))
          (emeld--dired-rerender-keep-point))
      (emeld-dired-visit))))

(defun emeld-dired-visit ()
  "Open the file at point in this window; toggle a directory."
  (interactive)
  (let ((node (emeld--dired-node-at-point)))
    (if (emeld-node-is-dir node)
        (emeld-dired-toggle-node)
      (find-file (emeld-node-left-path node)))))

(defun emeld-dired-visit-other-window ()
  "Open the file at point in another window; toggle a directory."
  (interactive)
  (let ((node (emeld--dired-node-at-point)))
    (if (emeld-node-is-dir node)
        (emeld-dired-toggle-node)
      (find-file-other-window (emeld-node-left-path node)))))

(defun emeld-dired-mouse-1 (event)
  "Act on the node clicked by EVENT (open file / toggle directory)."
  (interactive "e")
  (mouse-set-point event)
  (emeld-dired-visit))

(defun emeld-dired-next-line ()
  "Move to the next file line."
  (interactive)
  (forward-line 1)
  (if (eobp) (forward-line -1) (back-to-indentation)))

(defun emeld-dired-previous-line ()
  "Move to the previous file line."
  (interactive)
  (forward-line -1)
  (if (bobp) (forward-line 1) (back-to-indentation)))

(defun emeld-dired-up-directory ()
  "Re-root the tree at the parent directory."
  (interactive)
  (let* ((here emeld--dired-root)
         (parent (file-name-directory (directory-file-name here))))
    (if (string= parent here)
        (message "Already at the filesystem root")
      (setq emeld--dired-root (file-name-as-directory parent)
            default-directory emeld--dired-root)
      (emeld--dired-build)
      ;; Reveal and select the directory we came up from.
      (emeld--dired-node-for here)
      (emeld--dired-render)
      (emeld--dired-goto-path here))))

;;;; ─── Expansion bulk operations ──────────────────────────────────

(defun emeld--dired-walk-expand (node)
  "Populate and expand NODE and all its descendant directories from disk."
  (when (emeld-node-is-dir node)
    (emeld--dired-populate node)
    (setf (emeld-node-expanded node) t)
    (mapc #'emeld--dired-walk-expand (emeld-node-children node))))

(defun emeld-dired-collapse-all ()
  "Collapse every directory in the tree."
  (interactive)
  (when emeld--root-node
    (dolist (c (emeld-node-children emeld--root-node))
      (emeld--walk-set-expanded c nil))
    (emeld--dired-rerender-keep-point)))

(defun emeld-dired-expand-all ()
  "Expand every directory in the tree, loading children from disk as needed."
  (interactive)
  (when emeld--root-node
    (mapc #'emeld--dired-walk-expand (emeld-node-children emeld--root-node))
    (emeld--dired-rerender-keep-point)))

;;;; ─── File operations ────────────────────────────────────────────

(defun emeld--dired-after-op (&optional reveal)
  "Rebuild the tree from disk, restoring expansion, then optionally REVEAL a path."
  (emeld--dired-prune-missing-marks)
  (let ((expanded (emeld--dired-expanded-paths emeld--root-node))
        (point-path (or reveal (emeld--dired-path-at-point))))
    (emeld--dired-build)
    (dolist (p expanded) (emeld--dired-node-for p t))
    (when reveal (emeld--dired-node-for reveal))
    (emeld--dired-render)
    (when point-path (emeld--dired-goto-path point-path))))

(defun emeld-dired-refresh ()
  "Rebuild the tree from disk, restoring expansion and point."
  (interactive)
  (emeld--dired-after-op))

(defun emeld-dired-do-delete ()
  "Delete the marked files (or the file at point) after confirmation."
  (interactive)
  (let ((targets (emeld--dired-targets)))
    (when (yes-or-no-p (format "Delete %d file(s)? " (length targets)))
      (let ((count 0) (errors 0))
        (dolist (path targets)
          (condition-case nil
              (progn (emeld--delete-one path)
                     (emeld--dired-set-mark path nil)
                     (cl-incf count))
            (error (cl-incf errors))))
        (emeld--dired-after-op)
        (message "Deleted %d file(s)%s" count
                 (if (> errors 0) (format " (%d errors)" errors) ""))))))

(defun emeld-dired-do-flagged-delete ()
  "Delete all files flagged for deletion (`D')."
  (interactive)
  (let ((flagged (emeld--dired-marked-paths emeld--dired-flag-char)))
    (if (null flagged)
        (message "(No deletions requested)")
      (when (yes-or-no-p (format "Delete %d flagged file(s)? " (length flagged)))
        (let ((count 0) (errors 0))
          (dolist (path flagged)
            (condition-case nil
                (progn (emeld--delete-one path)
                       (emeld--dired-set-mark path nil)
                       (cl-incf count))
              (error (cl-incf errors))))
          (emeld--dired-after-op)
          (message "Deleted %d flagged file(s)%s" count
                   (if (> errors 0) (format " (%d errors)" errors) "")))))))

(defun emeld--dired-copy-one (source dest)
  "Copy SOURCE to DEST, recursively for directories."
  (if (file-directory-p source)
      (copy-directory source dest t t t)
    (copy-file source dest 1)))

(defun emeld-dired-do-copy ()
  "Copy the marked files (or file at point) to a prompted destination."
  (interactive)
  (let* ((targets (emeld--dired-targets))
         (multi (> (length targets) 1))
         (dest (expand-file-name
                (read-file-name
                 (if multi (format "Copy %d files to directory: " (length targets))
                   (format "Copy %s to: " (file-name-nondirectory
                                           (directory-file-name (car targets)))))
                 emeld--dired-root
                 (and (not multi) (file-name-nondirectory (car targets)))))))
    (when (and multi (not (file-directory-p dest)))
      (user-error "Destination must be an existing directory when copying many files"))
    (dolist (source targets)
      (let ((to (if (file-directory-p dest)
                    (expand-file-name (file-name-nondirectory
                                       (directory-file-name source))
                                      dest)
                  dest)))
        (emeld--dired-copy-one source to)))
    ;; Keep point on the source and advance to the next item (like Dired).
    (emeld--dired-after-op)
    (emeld-dired-next-line)
    (message "Copied %d file(s)" (length targets))))

(defun emeld-dired-do-rename ()
  "Rename/move the marked files (or file at point) to a prompted destination."
  (interactive)
  (let* ((targets (emeld--dired-targets))
         (multi (> (length targets) 1))
         (dest (expand-file-name
                (read-file-name
                 (if multi (format "Move %d files to directory: " (length targets))
                   (format "Rename %s to: " (file-name-nondirectory
                                             (directory-file-name (car targets)))))
                 emeld--dired-root
                 (and (not multi) (file-name-nondirectory (car targets))))))
         (last nil))
    (when (and multi (not (file-directory-p dest)))
      (user-error "Destination must be an existing directory when moving many files"))
    (dolist (source targets)
      (let ((to (if (file-directory-p dest)
                    (expand-file-name (file-name-nondirectory
                                       (directory-file-name source))
                                      dest)
                  dest)))
        (rename-file source to 1)
        (emeld--dired-set-mark source nil)
        (setq last to)))
    (emeld--dired-after-op (and last (string-prefix-p emeld--dired-root
                                                      (expand-file-name last))
                                last))
    (message "Renamed %d file(s)" (length targets))))

(defun emeld-dired-create-directory (dir)
  "Create directory DIR (relative to the directory at point) and reveal it."
  (interactive
   (let* ((node (get-text-property (point) 'emeld-node))
          (base (cond ((null node) emeld--dired-root)
                      ((emeld-node-is-dir node) (emeld-node-left-path node))
                      (t (file-name-directory (emeld-node-left-path node))))))
     (list (read-file-name "Create directory: " (file-name-as-directory base)))))
  (let ((dir (expand-file-name dir)))
    (make-directory dir t)
    (emeld--dired-after-op dir)
    (message "Created %s" (abbreviate-file-name dir))))

(defun emeld-dired-create-file (file)
  "Create an empty FILE (relative to the directory at point) and visit it."
  (interactive
   (let* ((node (get-text-property (point) 'emeld-node))
          (base (cond ((null node) emeld--dired-root)
                      ((emeld-node-is-dir node) (emeld-node-left-path node))
                      (t (file-name-directory (emeld-node-left-path node))))))
     (list (read-file-name "Create file: " (file-name-as-directory base)))))
  (let ((file (expand-file-name file)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (write-region "" nil file))
    (emeld--dired-after-op file)
    (find-file file)))

;;;; ─── Display toggles ────────────────────────────────────────────

(defun emeld-dired-toggle-hidden ()
  "Toggle showing dotfiles in this tree."
  (interactive)
  (setq-local emeld-dired-show-hidden (not emeld-dired-show-hidden))
  (emeld--dired-after-op)
  (message "Dotfiles: %s" (if emeld-dired-show-hidden "shown" "hidden")))

(defun emeld-dired-cycle-density ()
  "Cycle this tree's density: compact -> extreme -> wide -> compact."
  (interactive)
  (setq-local emeld-tree-density (emeld--tree-density-next emeld-tree-density))
  (emeld--dired-rerender-keep-point)
  (message "Tree density: %s" emeld-tree-density))

(defun emeld-dired-toggle-tree-lines ()
  "Toggle the tree-line guides in this tree."
  (interactive)
  (setq-local emeld-show-tree-lines (not emeld-show-tree-lines))
  (emeld--dired-rerender-keep-point)
  (message "Tree-lines: %s" (if emeld-show-tree-lines "on" "off")))

;;;; ─── Mode ────────────────────────────────────────────────────────

(defvar emeld-dired-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation / visiting
    (define-key map (kbd "RET")     #'emeld-dired-visit)
    (define-key map (kbd "f")       #'emeld-dired-visit)
    (define-key map (kbd "e")       #'emeld-dired-visit)
    (define-key map (kbd "o")       #'emeld-dired-visit-other-window)
    (define-key map (kbd "TAB")     #'emeld-dired-toggle-node)
    (define-key map (kbd "<tab>")   #'emeld-dired-toggle-node)
    (define-key map [mouse-1]       #'emeld-dired-mouse-1)
    (define-key map (kbd "n")       #'emeld-dired-next-line)
    (define-key map (kbd "p")       #'emeld-dired-previous-line)
    (define-key map (kbd "C-n")     #'emeld-dired-next-line)
    (define-key map (kbd "C-p")     #'emeld-dired-previous-line)
    (define-key map (kbd "^")       #'emeld-dired-up-directory)
    (define-key map (kbd "[")       #'emeld-dired-collapse-all)
    (define-key map (kbd "]")       #'emeld-dired-expand-all)
    ;; Marking
    (define-key map (kbd "m")       #'emeld-dired-mark)
    (define-key map (kbd "u")       #'emeld-dired-unmark)
    (define-key map (kbd "DEL")     #'emeld-dired-unmark-backward)
    (define-key map (kbd "U")       #'emeld-dired-unmark-all)
    (define-key map (kbd "t")       #'emeld-dired-toggle-marks)
    (define-key map (kbd "d")       #'emeld-dired-flag-delete)
    ;; File operations
    (define-key map (kbd "x")       #'emeld-dired-do-flagged-delete)
    (define-key map (kbd "D")       #'emeld-dired-do-delete)
    (define-key map (kbd "C")       #'emeld-dired-do-copy)
    (define-key map (kbd "R")       #'emeld-dired-do-rename)
    (define-key map (kbd "+")       #'emeld-dired-create-directory)
    (define-key map (kbd "M-+")     #'emeld-dired-create-file)
    (define-key map (kbd "g")       #'emeld-dired-refresh)
    ;; Display toggles
    (define-key map (kbd "h")       #'emeld-dired-toggle-hidden)
    (define-key map (kbd "z")       #'emeld-dired-cycle-density)
    (define-key map (kbd "T")       #'emeld-dired-toggle-tree-lines)
    (define-key map (kbd "q")       #'quit-window)
    map)
  "Keymap for `emeld-dired-mode'.")

(define-derived-mode emeld-dired-mode special-mode "Emeld-Dired"
  "Major mode for the tree-navigable, Dired-like emeld file manager.

\\<emeld-dired-mode-map>
Navigation:
  \\[emeld-dired-next-line]/\\[emeld-dired-previous-line]   next/previous line
  \\[emeld-dired-toggle-node]   expand/collapse a directory (or visit a file)
  \\[emeld-dired-visit] / \\[emeld-dired-visit-other-window]   visit file (this / other window)
  \\[emeld-dired-up-directory]   re-root at the parent directory
  \\[emeld-dired-collapse-all]/\\[emeld-dired-expand-all]   collapse/expand all

Marking (Dired-style):
  \\[emeld-dired-mark]   mark      \\[emeld-dired-unmark]   unmark    \\[emeld-dired-unmark-all]   unmark all
  \\[emeld-dired-flag-delete]   flag for deletion           \\[emeld-dired-toggle-marks]   toggle marks

Operations (act on marked files, else the file at point):
  \\[emeld-dired-do-delete]   delete           \\[emeld-dired-do-flagged-delete]   delete flagged
  \\[emeld-dired-do-copy]   copy             \\[emeld-dired-do-rename]   rename/move
  \\[emeld-dired-create-directory]   create directory \\[emeld-dired-create-file]   create file
  \\[emeld-dired-refresh]   refresh          \\[emeld-dired-toggle-hidden]   toggle dotfiles

\\{emeld-dired-mode-map}"
  (buffer-disable-undo)
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t)
  (make-local-variable 'emeld--dired-root)
  (make-local-variable 'emeld--dired-marks)
  (make-local-variable 'emeld--root-node)
  (make-local-variable 'emeld-dired-show-hidden)
  (make-local-variable 'emeld-show-tree-lines)
  (setq emeld--dired-marks (make-hash-table :test 'equal)))

;;;; ─── Entry points ───────────────────────────────────────────────

(defun emeld--dired-buffer-name (root)
  "Return the buffer name for an emeld-dired tree rooted at ROOT."
  (format "*emeld-dired: %s*" (abbreviate-file-name root)))

(defun emeld--dired-current-directory ()
  "Return the directory the current buffer is \"in\".
The Dired directory when in Dired, else `default-directory'."
  (cond ((derived-mode-p 'dired-mode)
         (or (and (fboundp 'dired-current-directory) (dired-current-directory))
             default-directory))
        (t default-directory)))

(defun emeld--dired-open (root)
  "Open (or rebuild) the emeld-dired tree rooted at ROOT in the current window."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (buf (get-buffer-create (emeld--dired-buffer-name root))))
    (with-current-buffer buf
      (unless (derived-mode-p 'emeld-dired-mode) (emeld-dired-mode))
      (setq emeld--dired-root root
            default-directory root)
      (emeld--dired-build)
      (emeld--dired-render))
    (pop-to-buffer-same-window buf)
    buf))

;;;###autoload
(defun emeld-dired (&optional arg)
  "Show the current Dired buffer (or directory) as a navigable emeld tree.

Roots at the current Dired directory when invoked from Dired, otherwise at
`default-directory'.  With a prefix ARG, prompt for the directory.  The tree
behaves like Dired — mark files (\\<emeld-dired-mode-map>\\[emeld-dired-mark]),
flag for deletion (\\[emeld-dired-flag-delete]), copy (\\[emeld-dired-do-copy]),
rename (\\[emeld-dired-do-rename]), delete (\\[emeld-dired-do-delete]) — but
directories fold open and closed (\\[emeld-dired-toggle-node]) so you navigate a
tree instead of one flat listing."
  (interactive "P")
  (let ((root (if arg
                  (read-directory-name "Emeld-dired root: ")
                (emeld--dired-current-directory))))
    (emeld--dired-open root)))

;;;###autoload
(defun emeld-dired-find (dir)
  "Open a navigable Dired-like emeld tree for directory DIR."
  (interactive "DEmeld-dired directory: ")
  (emeld--dired-open dir))

(provide 'emeld-dired)
;;; emeld-dired.el ends here
