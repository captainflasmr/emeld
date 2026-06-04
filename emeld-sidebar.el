;;; emeld-sidebar.el --- Treemacs-style file-tree sidebar for emeld -*- lexical-binding: t; -*-

;;; Commentary:
;; A docked side window that renders ONE directory tree (not a diff), built on
;; the same `emeld-node' model and rendering helpers as the comparison view.
;; Entry point: `M-x emeld-sidebar'.  See `emeld-core.el' for the shared core.

;;; Code:

(require 'cl-lib)
(require 'emeld-core)

;;;; ─── Sidebar (treemacs-style single-tree file explorer) ──────────
;;
;; A docked side window that renders ONE directory tree (not a diff),
;; reusing the `emeld-node' model, the expand/collapse state, the tree-line
;; drawing and the `diff-*'/`emeld-*' faces.  It roots at the project (git
;; toplevel / `project.el' / the file's directory), reveals and highlights
;; the active file as you switch buffers (`emeld-sidebar-follow-mode'), and
;; opens files in the main window.  Children are loaded lazily per directory
;; (the node's `scanned' slot doubles as "children loaded?").

(defcustom emeld-sidebar-width 35
  "Width, in columns, of the emeld sidebar side window."
  :type 'integer
  :group 'emeld)

(defcustom emeld-sidebar-position 'left
  "Frame side the emeld sidebar docks to."
  :type '(choice (const left) (const right))
  :group 'emeld)

(defcustom emeld-sidebar-show-hidden nil
  "When non-nil, show dotfiles in the sidebar; otherwise hide them.
Toggle per-sidebar with \\<emeld-sidebar-mode-map>\\[emeld-sidebar-toggle-hidden]."
  :type 'boolean
  :group 'emeld)

(defcustom emeld-sidebar-follow t
  "When non-nil, opening the sidebar also enables `emeld-sidebar-follow-mode'."
  :type 'boolean
  :group 'emeld)

(defconst emeld--sidebar-buffer-name "*emeld-sidebar*"
  "Name of the singleton emeld sidebar buffer.")

(defvar-local emeld--sidebar-root nil
  "Absolute root directory (with trailing slash) of the sidebar tree.")
(defvar-local emeld--sidebar-current nil
  "Absolute path of the file currently revealed/highlighted, or nil.")
(defvar-local emeld--sidebar-hl-overlay nil
  "Overlay marking the current file's line in the sidebar.
Used instead of `hl-line-mode' so the highlight tracks the active file even
when the sidebar window is not selected.")

(defface emeld-sidebar-highlight-face
  '((t :inherit hl-line :extend t))
  "Face for the line of the currently-tracked file in the sidebar."
  :group 'emeld)

(defun emeld--sidebar-project-root (&optional file)
  "Return the project root for FILE (or the current buffer) with a trailing slash.
Prefers the git work tree, then a `project.el' project, then the file's own
directory."
  (let* ((file (or file (buffer-file-name) default-directory))
         (dir (file-name-directory (expand-file-name file))))
    (file-name-as-directory
     (expand-file-name
      (or (emeld--git-toplevel dir)
          (when (fboundp 'project-current)
            (when-let ((proj (project-current nil dir)))
              (cond ((fboundp 'project-root) (project-root proj))
                    ((fboundp 'project-roots) (car (project-roots proj))))))
          dir)))))

(defun emeld--sidebar-visible-p (name)
  "Return non-nil when a child basename NAME should be shown in the sidebar.
Hides dotfiles unless `emeld-sidebar-show-hidden', and honours the
basename globs of every filter group that is active by default."
  (and (not (member name '("." "..")))
       (or emeld-sidebar-show-hidden (not (string-prefix-p "." name)))
       (not (cl-some
             (lambda (group)
               (and (emeld--group-active-default group)
                    (cl-some (lambda (glob)
                               (and (not (string-match-p "/" glob))
                                    (string-match-p (emeld--glob-to-regexp glob) name)))
                             (emeld--group-globs group))))
             emeld-filter-groups))))

(defun emeld--sidebar-dir-children (node)
  "Build NODE's immediate child nodes from disk: directories first, then files."
  (let ((dirs nil) (files nil))
    (dolist (abs (ignore-errors
                   (directory-files (emeld-node-left-path node) t
                                    directory-files-no-dot-files-regexp)))
      (let ((name (file-name-nondirectory abs)))
        (when (emeld--sidebar-visible-p name)
          (let ((child (emeld-node-create
                        :name name :left-path abs
                        :is-dir (file-directory-p abs)
                        :expanded nil :scanned nil :parent node)))
            (if (emeld-node-is-dir child) (push child dirs) (push child files))))))
    (cl-flet ((by-name (a b) (string-lessp (emeld-node-name a) (emeld-node-name b))))
      (append (sort dirs #'by-name) (sort files #'by-name)))))

(defun emeld--sidebar-populate (node)
  "Load NODE's children from disk when not already loaded (NODE's `scanned' slot)."
  (when (and (emeld-node-is-dir node) (not (emeld-node-scanned node)))
    (setf (emeld-node-children node) (emeld--sidebar-dir-children node)
          (emeld-node-scanned node) t)))

(defun emeld--sidebar-build ()
  "Build the root node and its first level of children for the sidebar."
  (let ((root (emeld-node-create
               :name (directory-file-name emeld--sidebar-root)
               :left-path emeld--sidebar-root
               :is-dir t :expanded t :scanned nil)))
    (emeld--sidebar-populate root)
    (setq emeld--root-node root)))

(defun emeld--sidebar-render-nodes (nodes prefix)
  "Render NODES under tree-line PREFIX into the sidebar buffer."
  (let ((count (length nodes)) (idx 0))
    (dolist (node nodes)
      (setq idx (1+ idx))
      (let* ((is-last (= idx count))
             (is-dir (emeld-node-is-dir node))
             (branch (emeld--tree-branch is-last))
             ;; Files sit flush after the branch (like the diff view); only
             ;; directories carry the expand toggle and folder prefix.
             (indicator (if is-dir
                            (concat (if (emeld-node-expanded node) "[-] " "[+] ")
                                    emeld-dir-prefix)
                          ""))
             (face (if is-dir 'emeld-dir-face 'default))
             (start (point)))
        (insert (propertize (concat prefix branch) 'face 'shadow))
        (insert indicator)
        (insert (propertize (emeld-node-name node) 'face face))
        (add-text-properties start (point) `(emeld-node ,node mouse-face highlight))
        (insert "\n")
        (when (and is-dir (emeld-node-expanded node) (emeld-node-children node))
          (emeld--sidebar-render-nodes
           (emeld-node-children node)
           (concat prefix (emeld--tree-continuation is-last))))))))

(defun emeld--sidebar-render ()
  "Render the sidebar tree, preserving the path at point and the highlight."
  (let ((inhibit-read-only t)
        (point-path (emeld--sidebar-path-at-point)))
    (erase-buffer)
    (insert (propertize (abbreviate-file-name emeld--sidebar-root)
                        'face 'emeld-header-face)
            "\n")
    (when emeld--root-node
      (emeld--sidebar-render-nodes (emeld-node-children emeld--root-node) ""))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (when point-path (emeld--sidebar-goto-path point-path))
    (emeld--sidebar-highlight emeld--sidebar-current)))

(defun emeld--sidebar-highlight (path)
  "Move the current-file highlight overlay onto PATH's line (or clear it).
Does not move point, so it works from any window."
  (when (overlayp emeld--sidebar-hl-overlay)
    (delete-overlay emeld--sidebar-hl-overlay)
    (setq emeld--sidebar-hl-overlay nil))
  (when path
    (save-excursion
      (goto-char (point-min))
      (let ((target (expand-file-name path)) (done nil))
        (while (and (not done) (not (eobp)))
          (let ((node (get-text-property (point) 'emeld-node)))
            (when (and node
                       (string= (expand-file-name (emeld-node-left-path node)) target))
              (setq emeld--sidebar-hl-overlay
                    (make-overlay (line-beginning-position)
                                  (min (point-max) (1+ (line-end-position)))))
              (overlay-put emeld--sidebar-hl-overlay 'face
                           'emeld-sidebar-highlight-face)
              (setq done t)))
          (forward-line 1))))))

(defun emeld--sidebar-path-at-point ()
  "Return the absolute path of the node at point, or nil."
  (let ((node (get-text-property (point) 'emeld-node)))
    (and node (emeld-node-left-path node))))

(defun emeld--sidebar-goto-path (path)
  "Move point to the line of the node whose path is PATH; return non-nil if found."
  (let ((target (expand-file-name path)) (found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found) (not (eobp)))
        (let ((node (get-text-property (point) 'emeld-node)))
          (when (and node
                     (string= (expand-file-name (emeld-node-left-path node)) target))
            (setq found (line-beginning-position))))
        (forward-line 1)))
    (when found (goto-char found) t)))

(defun emeld--sidebar-node-for (file &optional expand-target)
  "Descend the tree to FILE, populating and expanding every ancestor directory.
Return the node for FILE, or nil when it is outside the root or missing.
When EXPAND-TARGET is non-nil and the target is a directory, expand it too."
  (when (and emeld--root-node emeld--sidebar-root)
    (let ((file (expand-file-name file)))
      (when (string-prefix-p emeld--sidebar-root file)
        (let* ((rel (substring file (length emeld--sidebar-root)))
               (parts (split-string rel "/" t))
               (node emeld--root-node))
          (setf (emeld-node-expanded node) t)
          (catch 'done
            (dolist (part parts)
              (emeld--sidebar-populate node)
              (setf (emeld-node-expanded node) t)
              (let ((child (cl-find-if
                            (lambda (c) (string= (emeld-node-name c) part))
                            (emeld-node-children node))))
                (unless child (throw 'done nil))
                (setq node child)))
            (when (and expand-target (emeld-node-is-dir node))
              (emeld--sidebar-populate node)
              (setf (emeld-node-expanded node) t))
            node))))))

(defun emeld--sidebar-reveal (file)
  "Expand ancestors, highlight FILE and move point to it in the sidebar.
Updates the sidebar window's point too, so the view scrolls to FILE even
when the sidebar is not the selected window."
  (when (emeld--sidebar-node-for file)
    (setq emeld--sidebar-current (expand-file-name file))
    (emeld--sidebar-render)
    (when (emeld--sidebar-goto-path file)
      (let ((win (get-buffer-window emeld--sidebar-buffer-name t)))
        (when win (set-window-point win (point)))))))

(defun emeld--sidebar-expanded-paths (node)
  "Return the absolute paths of every expanded directory under NODE."
  (let (acc)
    (cl-labels ((walk (n)
                  (when (and (emeld-node-is-dir n) (emeld-node-expanded n))
                    (push (expand-file-name (emeld-node-left-path n)) acc)
                    (dolist (c (emeld-node-children n)) (walk c)))))
      (when node (walk node)))
    acc))

(defun emeld--sidebar-main-window ()
  "Return a window for displaying files: the most-recent non-sidebar window."
  (or (get-mru-window nil nil t)
      (next-window (selected-window) 'no-mini)))

;;;; Sidebar commands

(defun emeld-sidebar-toggle-node ()
  "Toggle the directory at point; on a file, visit it (\\[emeld-sidebar-visit])."
  (interactive)
  (let ((node (get-text-property (point) 'emeld-node)))
    (cond
     ((null node) (user-error "No node at point"))
     ((emeld-node-is-dir node)
      (emeld--sidebar-populate node)
      (setf (emeld-node-expanded node) (not (emeld-node-expanded node)))
      (let ((p (emeld--sidebar-path-at-point)))
        (emeld--sidebar-render)
        (when p (emeld--sidebar-goto-path p))))
     (t (emeld-sidebar-visit)))))

(defun emeld-sidebar-visit ()
  "Open the file at point in the main window and select it; toggle a directory."
  (interactive)
  (let ((node (get-text-property (point) 'emeld-node)))
    (cond
     ((null node) (user-error "No node at point"))
     ((emeld-node-is-dir node) (emeld-sidebar-toggle-node))
     (t (let ((win (emeld--sidebar-main-window)))
          (when win (select-window win))
          (find-file (emeld-node-left-path node)))))))

(defun emeld-sidebar-visit-no-select ()
  "Open the file at point in the main window without leaving the sidebar."
  (interactive)
  (let ((node (get-text-property (point) 'emeld-node)))
    (cond
     ((null node) (user-error "No node at point"))
     ((emeld-node-is-dir node) (emeld-sidebar-toggle-node))
     (t (let ((win (emeld--sidebar-main-window)))
          (when win
            (with-selected-window win
              (find-file (emeld-node-left-path node)))))))))

(defun emeld-sidebar-mouse-1 (event)
  "Act on the node clicked by EVENT (open file / toggle directory)."
  (interactive "e")
  (mouse-set-point event)
  (emeld-sidebar-visit))

(defun emeld-sidebar-next-line ()
  "Move to the next line in the sidebar."
  (interactive)
  (forward-line 1)
  (beginning-of-line))

(defun emeld-sidebar-previous-line ()
  "Move to the previous line in the sidebar."
  (interactive)
  (forward-line -1)
  (beginning-of-line))

(defun emeld-sidebar-collapse-all ()
  "Collapse every directory in the sidebar."
  (interactive)
  (when emeld--root-node
    (dolist (c (emeld-node-children emeld--root-node))
      (emeld--walk-set-expanded c nil))
    (emeld--sidebar-render)))

(defun emeld--sidebar-walk-expand (node)
  "Populate and expand NODE and all its descendant directories from disk."
  (when (emeld-node-is-dir node)
    (emeld--sidebar-populate node)
    (setf (emeld-node-expanded node) t)
    (mapc #'emeld--sidebar-walk-expand (emeld-node-children node))))

(defun emeld--sidebar-walk-dirs-only (node)
  "Expand NODE's intermediate dirs and collapse leaf dirs (no sub-dirs).
Populates from disk as it descends so the whole folder skeleton is known."
  (when (emeld-node-is-dir node)
    (emeld--sidebar-populate node)
    (setf (emeld-node-expanded node)
          (cl-some #'emeld-node-is-dir (emeld-node-children node)))
    (mapc #'emeld--sidebar-walk-dirs-only (emeld-node-children node))))

(defun emeld-sidebar-expand-all ()
  "Expand every directory in the sidebar, loading children from disk as needed."
  (interactive)
  (when emeld--root-node
    (mapc #'emeld--sidebar-walk-expand (emeld-node-children emeld--root-node))
    (emeld--sidebar-render)))

(defvar-local emeld--sidebar-cycle-state nil
  "Most recent state from `emeld-sidebar-cycle-visibility'.
One of `top', `dirs', `all', or nil.")

(defun emeld-sidebar-cycle-visibility ()
  "Cycle the sidebar's expansion in three steps, like the diff view's `S-TAB'.

Order:
- TOP: only the root's immediate child directories visible (rest collapsed).
- DIRS: every intermediate directory expanded, leaf directories collapsed so
  the folder skeleton shows without diving into the deepest folders.
- ALL: every directory expanded.

Repeated invocations cycle TOP -> DIRS -> ALL -> TOP."
  (interactive)
  (when emeld--root-node
    (let ((next (pcase emeld--sidebar-cycle-state
                  ('top 'dirs)
                  ('dirs 'all)
                  (_ 'top)))
          (children (emeld-node-children emeld--root-node)))
      (setq emeld--sidebar-cycle-state next)
      (pcase next
        ('top  (dolist (c children) (emeld--walk-set-expanded c nil))
               (message "Visibility: TOP (top-level dirs)"))
        ('dirs (mapc #'emeld--sidebar-walk-dirs-only children)
               (message "Visibility: DIRS (folder skeleton)"))
        ('all  (mapc #'emeld--sidebar-walk-expand children)
               (message "Visibility: ALL (everything)")))
      (emeld--sidebar-render))))

(defun emeld-sidebar-refresh ()
  "Rebuild the sidebar tree from disk, restoring expansion and point."
  (interactive)
  (let ((expanded (emeld--sidebar-expanded-paths emeld--root-node))
        (point-path (emeld--sidebar-path-at-point)))
    (emeld--sidebar-build)
    (dolist (p expanded) (emeld--sidebar-node-for p t))
    (emeld--sidebar-render)
    (when point-path (emeld--sidebar-goto-path point-path))))

(defun emeld-sidebar-toggle-hidden ()
  "Toggle showing dotfiles in this sidebar."
  (interactive)
  (setq-local emeld-sidebar-show-hidden (not emeld-sidebar-show-hidden))
  (emeld-sidebar-refresh)
  (message "Sidebar dotfiles: %s" (if emeld-sidebar-show-hidden "shown" "hidden")))

(defun emeld-sidebar-cycle-density ()
  "Cycle this sidebar's tree density: compact → extreme → wide → compact.
The setting is buffer-local to the sidebar, so it does not affect
`emeld-diff' buffers.  See `emeld-tree-density'."
  (interactive)
  (setq-local emeld-tree-density (emeld--tree-density-next emeld-tree-density))
  (emeld--sidebar-render)
  (message "Sidebar tree density: %s" emeld-tree-density))

(defun emeld-sidebar-toggle-tree-lines ()
  "Toggle the tree-line guides (`│├└') in this sidebar.
The setting is buffer-local to the sidebar, so it does not affect
`emeld-diff' buffers.  See `emeld-show-tree-lines'."
  (interactive)
  (setq-local emeld-show-tree-lines (not emeld-show-tree-lines))
  (emeld--sidebar-render)
  (message "Sidebar tree-lines: %s" (if emeld-show-tree-lines "on" "off")))

(defun emeld-sidebar-reveal-current ()
  "Reveal the file shown in the main window within the sidebar."
  (interactive)
  (let* ((win (emeld--sidebar-main-window))
         (file (and win (buffer-file-name (window-buffer win)))))
    (if file (emeld--sidebar-reveal file)
      (message "No file in the main window to reveal"))))

(defvar emeld-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")     #'emeld-sidebar-visit)
    (define-key map (kbd "f")       #'emeld-sidebar-visit)
    (define-key map (kbd "v")       #'emeld-sidebar-visit)
    (define-key map (kbd "TAB")     #'emeld-sidebar-toggle-node)
    (define-key map (kbd "<tab>")   #'emeld-sidebar-toggle-node)
    (define-key map [mouse-1]       #'emeld-sidebar-mouse-1)
    (define-key map (kbd "o")       #'emeld-sidebar-visit-no-select)
    (define-key map (kbd "n")       #'emeld-sidebar-next-line)
    (define-key map (kbd "p")       #'emeld-sidebar-previous-line)
    (define-key map (kbd "g")       #'emeld-sidebar-refresh)
    (define-key map (kbd "[")       #'emeld-sidebar-collapse-all)
    (define-key map (kbd "]")       #'emeld-sidebar-expand-all)
    (define-key map (kbd "<backtab>") #'emeld-sidebar-cycle-visibility)
    (define-key map (kbd "S-TAB")     #'emeld-sidebar-cycle-visibility)
    (define-key map (kbd "h")       #'emeld-sidebar-toggle-hidden)
    (define-key map (kbd "z")       #'emeld-sidebar-cycle-density)
    (define-key map (kbd "T")       #'emeld-sidebar-toggle-tree-lines)
    (define-key map (kbd "F")       #'emeld-sidebar-follow-mode)
    (define-key map (kbd ".")       #'emeld-sidebar-reveal-current)
    (define-key map (kbd "q")       #'quit-window)
    map)
  "Keymap for `emeld-sidebar-mode'.")

(define-derived-mode emeld-sidebar-mode special-mode "Emeld-Sidebar"
  "Major mode for the emeld treemacs-style file-tree sidebar.
\\{emeld-sidebar-mode-map}"
  (buffer-disable-undo)
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t)
  (make-local-variable 'emeld--sidebar-root)
  (make-local-variable 'emeld--sidebar-current)
  (make-local-variable 'emeld--sidebar-hl-overlay)
  (make-local-variable 'emeld--root-node)
  (make-local-variable 'emeld-sidebar-show-hidden)
  (make-local-variable 'emeld-show-tree-lines)
  ;; The sidebar is narrow, so default to the most condensed density.
  (setq-local emeld-tree-density 'extreme))

(defun emeld--sidebar-get-buffer (root)
  "Return the sidebar buffer, (re)building its tree rooted at ROOT."
  (let ((buf (get-buffer-create emeld--sidebar-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'emeld-sidebar-mode) (emeld-sidebar-mode))
      (setq emeld--sidebar-root root
            default-directory root)
      (emeld--sidebar-build)
      (emeld--sidebar-render))
    buf))

;;;###autoload
(defun emeld-sidebar (&optional arg)
  "Toggle a treemacs-style file-tree sidebar for the current project.
Roots at the git/`project.el' root of the current file (or its directory)
and docks a dedicated side window on `emeld-sidebar-position'.  With a
prefix ARG, prompt for the root directory instead.  Run again to close it.

Opening the sidebar keeps focus in the current buffer.  The active file is
revealed and highlighted; as you switch buffers it keeps up when
`emeld-sidebar-follow-mode' is on (enabled here when `emeld-sidebar-follow'
is non-nil).  RET / `f' / `v' open a file (or toggle
a directory), TAB expands/collapses, `o' opens without leaving the sidebar,
`g' refreshes, `.' reveals the main window's file, `F' toggles follow-mode,
`q' closes it."
  (interactive "P")
  (let ((win (get-buffer-window emeld--sidebar-buffer-name)))
    (if (and win (not arg))
        (delete-window win)
      (let* ((root (if arg
                       (file-name-as-directory
                        (expand-file-name (read-directory-name "Sidebar root: ")))
                     (emeld--sidebar-project-root)))
             (current (buffer-file-name))
             (buf (emeld--sidebar-get-buffer root))
             (window (display-buffer-in-side-window
                      buf `((side . ,emeld-sidebar-position)
                            (slot . 0)
                            (window-width . ,emeld-sidebar-width)
                            (preserve-size . (t . nil))))))
        (when emeld-sidebar-follow (emeld-sidebar-follow-mode 1))
        (when window
          (set-window-parameter window 'no-delete-other-windows t)
          (set-window-dedicated-p window t)
          ;; Keep focus in the file the user came from; reveal in the sidebar
          ;; without selecting it (`emeld--sidebar-reveal' moves the sidebar
          ;; window's point and highlight, no selection needed).
          (when current
            (with-current-buffer buf (emeld--sidebar-reveal current))))))))

(defun emeld--sidebar-follow (&optional _frame)
  "Reveal the active window's file in the sidebar (follow-mode hook).
Fires on both window-selection and window-buffer changes, so it tracks
switching windows *and* opening a file in the current window."
  (let ((sb (get-buffer-window emeld--sidebar-buffer-name t)))
    (when (and sb (not (eq (selected-window) sb)))
      (let ((file (buffer-file-name (window-buffer (selected-window))))
            (sb-buf (window-buffer sb)))
        (when file
          (let ((file (expand-file-name file)))
            (with-current-buffer sb-buf
              (when (and emeld--sidebar-root
                         (string-prefix-p emeld--sidebar-root file)
                         (not (equal file emeld--sidebar-current)))
                (emeld--sidebar-reveal file)))))))))

;;;###autoload
(define-minor-mode emeld-sidebar-follow-mode
  "Global minor mode: the emeld sidebar reveals and highlights the active file.
When on, displaying a file (by switching windows or opening it in the
current window) under the sidebar root expands the path to it, moves the
sidebar's point onto it and highlights its line."
  :global t
  :group 'emeld
  (if emeld-sidebar-follow-mode
      (progn
        (add-hook 'window-selection-change-functions #'emeld--sidebar-follow)
        (add-hook 'window-buffer-change-functions #'emeld--sidebar-follow))
    (remove-hook 'window-selection-change-functions #'emeld--sidebar-follow)
    (remove-hook 'window-buffer-change-functions #'emeld--sidebar-follow)))

(provide 'emeld-sidebar)
;;; emeld-sidebar.el ends here
