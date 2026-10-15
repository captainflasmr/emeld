;;; emeld-git.el --- Git revision comparison for emeld -*- lexical-binding: t; -*-

;;; Commentary:
;; Compare two git revisions (or a revision against the working tree / index)
;; as the same hierarchical tree as the directory diff, plus commit-by-commit
;; history stepping.  Entry points: `emeld-git-diff', `emeld-git-step-history'.
;; Built on `emeld-core.el' (shared node model, scan/render, git utilities).

;;; Code:

(require 'cl-lib)
(require 'emeld-core)

(defcustom emeld-git-history-limit 1000
  "Maximum number of commits to load when stepping through git history.
`emeld-git-step-history' walks the first-parent chain from the starting
revision; this caps how far back it loads.  Stepping past the loaded edge
reports the limit rather than loading more — re-run from an older commit
to continue."
  :type 'integer
  :group 'emeld)

(defcustom emeld-git-history-auto-expand t
  "When non-nil, auto-expand changed directories after a history step.
Each step of `emeld-git-history-older' / `emeld-git-history-newer'
rebuilds the tree, then expands the directories leading to a change (as
`emeld-expand-changes' does) so the whole commit's hierarchy is opened
out without a manual expand.  Set to nil to keep the saved expansion
state across steps instead."
  :type 'boolean
  :group 'emeld)

;;;###autoload
(defun emeld-git-diff (repo left-rev right-rev)
  "Compare two git revisions of REPO side-by-side as a tree of changes.
REPO is a git work-tree root.  LEFT-REV and RIGHT-REV are revisions (a
commit, branch or tag), or the symbols `working' (the working tree) or
`staged' (the index).  Interactively, REPO is the work tree containing
`default-directory', LEFT-REV defaults to HEAD and RIGHT-REV to the
working tree.

The hierarchy of added, deleted and modified files renders with the same
tree as `emeld-diff'.  File content comes from `git show', so opening a
file shows it at its revision and `e' ediffs the two revisions; copy and
delete are disabled, since history cannot be written to."
  (interactive
   (let* ((repo (or (emeld--git-toplevel default-directory)
                    (user-error "Not inside a git work tree")))
          (left (emeld--read-git-rev "Left (old) revision: " repo "HEAD"))
          (right (emeld--read-git-rev "Right (new) revision: " repo 'working)))
     (list repo left right)))
  (unless (emeld--git-work-tree-p repo)
    (user-error "Not a git work tree: %s" repo))
  (let* ((repo (file-name-as-directory (expand-file-name repo)))
         (lname (emeld--git-rev-label left-rev))
         (rname (emeld--git-rev-label right-rev))
         (buf (get-buffer-create (format "*emeld-git: %s <-> %s*" lname rname))))
    (with-current-buffer buf
      (emeld-mode)
      (setq default-directory repo)
      (setq emeld--scan-max-depth nil)
      (setq emeld--git-mode t
            emeld--git-repo repo
            emeld--git-left-rev left-rev
            emeld--git-right-rev right-rev)
      (emeld--ensure-dir-locals default-directory)
      (emeld--apply-dir-locals)
      (setq emeld--active-filter-groups
            (cl-loop for g in emeld-filter-groups
                     when (emeld--group-active-default g)
                     collect (car g)))
      (setq emeld--marked-paths (make-hash-table :test 'equal))
      ;; The roots double as the repo root: harmless for any incidental path
      ;; join, and lets directory-oriented helpers fall back gracefully.
      (setq emeld--left-root repo
            emeld--right-root repo)
      (setq emeld--root-node (emeld-node-create
                              :name lname
                              :name-right rname
                              :left-path repo
                              :right-path repo
                              :is-dir t
                              :expanded t))
      (setf (emeld-node-children emeld--root-node) nil)
      (emeld--render))
    (switch-to-buffer buf)
    (emeld-refresh)))

;;;###autoload
(defun emeld-git-step-history (repo start)
  "Step through git history from START, one commit at a time.
Each step compares a commit against its first parent, so the buffer shows
what that commit changed.  Move to the older commit with \\`,'
\(`emeld-git-history-older') and back toward START (newer) with \\`.'
\(`emeld-git-history-newer').

REPO is a git work tree; START (default HEAD) is the newest commit of the
walk.  Only the first-parent chain is followed, and at most
`emeld-git-history-limit' commits are loaded.  All the usual git-mode
commands work on whichever commit is showing — view a file at its
revision, ediff against the parent, fold the commit message, etc."
  (interactive
   (let* ((repo (or (emeld--git-toplevel default-directory)
                    (user-error "Not inside a git work tree")))
          (start (emeld--read-git-rev "Step through history from: " repo "HEAD")))
     (list repo start)))
  (unless (emeld--git-work-tree-p repo)
    (user-error "Not a git work tree: %s" repo))
  (when (memq start '(working staged)) (setq start "HEAD"))
  (let* ((repo (file-name-as-directory (expand-file-name repo)))
         (history (let ((emeld--git-repo repo))
                    (emeld--git-history-list start emeld-git-history-limit))))
    (unless history
      (user-error "No commits found from %s" (emeld--git-rev-label start)))
    (let ((buf (get-buffer-create
                (format "*emeld-git-history: %s*"
                        (file-name-nondirectory (directory-file-name repo))))))
      (with-current-buffer buf
        (emeld-mode)
        (setq default-directory repo)
        (setq emeld--scan-max-depth nil)
        (setq emeld--git-mode t
              emeld--git-repo repo
              emeld--git-history history
              emeld--git-history-index 0)
        (emeld--ensure-dir-locals default-directory)
        (emeld--apply-dir-locals)
        (setq emeld--active-filter-groups
              (cl-loop for g in emeld-filter-groups
                       when (emeld--group-active-default g)
                       collect (car g)))
        (setq emeld--marked-paths (make-hash-table :test 'equal))
        (setq emeld--left-root repo
              emeld--right-root repo)
        (setq emeld--root-node (emeld-node-create
                                :name "" :name-right ""
                                :left-path repo :right-path repo
                                :is-dir t :expanded t))
        (setf (emeld-node-children emeld--root-node) nil))
      (switch-to-buffer buf)
      (with-current-buffer buf
        (emeld--git-history-apply)))))

(defconst emeld--git-working-label "[working tree]"
  "Completion label standing in for the working tree (the `working' rev).")
(defconst emeld--git-index-label "[index]"
  "Completion label standing in for the index (the `staged' rev).")
(defun emeld--git-commit-info (rev)
  "Return commit metadata for REV as a plist, or nil.
The plist has :sha (abbreviated), :author, :date, :subject and :body.
Returns nil when REV is the working tree or index, or does not resolve to
a commit.  Reads the buffer-local `emeld--git-repo' as the work tree."
  (when (and (stringp rev) emeld--git-repo
             (not (string= rev emeld--git-empty-tree)))
    (let ((default-directory emeld--git-repo))
      (with-temp-buffer
        (when (eq 0 (process-file "git" nil t nil "show" "-s" "--no-color"
                                  "--date=iso"
                                  "--format=%h%n%an <%ae>%n%ad%n%s%n%b" rev))
          (goto-char (point-min))
          (cl-flet ((line ()
                      (prog1 (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position))
                        (forward-line 1))))
            (let* ((sha (line))
                   (author (line))
                   (date (line))
                   (subject (line))
                   (body (string-trim-right
                          (buffer-substring-no-properties (point) (point-max)))))
              (list :sha sha :author author :date date
                    :subject subject :body body))))))))

(defun emeld--git-revisions (repo)
  "Return candidate revision strings for REPO.
Local/remote branches and tags first, then recent commits as
\"SHA SUBJECT\" lines (the SHA is extracted on selection)."
  (let ((default-directory (file-name-as-directory (expand-file-name repo)))
        (refs nil)
        (commits nil))
    (with-temp-buffer
      (when (eq 0 (process-file "git" nil t nil "for-each-ref"
                                "--format=%(refname:short)"
                                "refs/heads" "refs/tags" "refs/remotes"))
        (setq refs (split-string (buffer-string) "\n" t))))
    (with-temp-buffer
      (when (eq 0 (process-file "git" nil t nil "log" "--oneline"
                                "--no-color" "-n" "50"))
        (setq commits (split-string (buffer-string) "\n" t))))
    (append (list "HEAD") refs commits)))

(defun emeld--read-git-rev (prompt repo &optional default)
  "Read a git revision in REPO via completion, returning a rev or symbol.
PROMPT is shown to the user; DEFAULT pre-fills the minibuffer.  Choosing
the working-tree or index entry returns the symbol `working' or `staged';
otherwise a revision string is returned (a \"SHA SUBJECT\" commit line is
reduced to its SHA)."
  (let* ((cands (append (list emeld--git-working-label emeld--git-index-label)
                        (emeld--git-revisions repo)))
         (default-str (cond ((eq default 'working) emeld--git-working-label)
                            ((eq default 'staged) emeld--git-index-label)
                            ((stringp default) default)))
         (choice (completing-read prompt cands nil nil nil nil default-str)))
    (cond
     ((string= choice emeld--git-working-label) 'working)
     ((string= choice emeld--git-index-label) 'staged)
     ((string-match "\\`\\([0-9a-fA-F]+\\) " choice) (match-string 1 choice))
     (t (string-trim choice)))))

(defun emeld--git-diff-command ()
  "Return the async `git diff --name-status -z' argv for the buffer's rev pair."
  (append (list "git" "-C" (directory-file-name emeld--git-repo)
                "diff" "--name-status" "-z" "--no-color"
                "--find-renames" "--find-copies")
          (emeld--whitespace-args)
          (emeld--git-rev-args emeld--git-left-rev emeld--git-right-rev)))

(defun emeld--git-history-list (start limit)
  "Return up to LIMIT first-parent commit SHAs from START, newest first.
Reads the buffer-local `emeld--git-repo'."
  (let ((default-directory emeld--git-repo))
    (with-temp-buffer
      (when (eq 0 (process-file "git" nil t nil "rev-list" "--first-parent"
                                (format "--max-count=%d" (max 1 limit)) start))
        (split-string (buffer-string) "\n" t)))))

(defun emeld--git-parent-rev (sha)
  "Return SHA's first parent, or the empty-tree object when SHA is a root commit."
  (let ((default-directory emeld--git-repo))
    (with-temp-buffer
      (if (eq 0 (process-file "git" nil t nil "rev-parse" "--verify" "-q"
                              (concat sha "^")))
          (string-trim (buffer-string))
        emeld--git-empty-tree))))

(defun emeld--git-resolve-head ()
  "Return the full SHA the repo's HEAD points at, or nil.
Reads the buffer-local `emeld--git-repo'."
  (when emeld--git-repo
    (let ((default-directory emeld--git-repo))
      (with-temp-buffer
        (and (eq 0 (process-file "git" nil t nil
                                 "rev-parse" "--verify" "-q" "HEAD"))
             (let ((s (string-trim (buffer-string))))
               (and (not (string-empty-p s)) s)))))))

(defun emeld--git-history-head-p ()
  "Return non-nil when the newest commit in the walk is the current HEAD.
Only then does a working-tree slot make sense above it, since otherwise
the working tree would be compared against an arbitrary older commit."
  (let ((head (car emeld--git-history)))
    (and head (equal head (emeld--git-resolve-head)))))

(defun emeld--git-history-apply ()
  "Set the rev pair from `emeld--git-history-index' and re-scan.
The indexed commit becomes the right (new) side and its parent the left
\(old) side, so the buffer shows what that commit changed.  The virtual
index -1 is the working-tree slot: the newest commit on the left and the
working tree on the right, showing the uncommitted changes on top of it."
  (let* ((h emeld--git-history)
         (n (length h))
         (i (max -1 (min emeld--git-history-index (1- n))))
         (working (< i 0))
         (right (if working 'working (nth i h)))
         (left (cond (working (car h))
                     ((< (1+ i) n) (nth (1+ i) h))
                     (t (emeld--git-parent-rev right)))))
    (setq emeld--git-history-index i
          emeld--git-left-rev left
          emeld--git-right-rev right
          emeld--git-expand-changes-pending emeld-git-history-auto-expand)
    (setf (emeld-node-name emeld--root-node) (emeld--git-rev-label left)
          (emeld-node-name-right emeld--root-node) (emeld--git-rev-label right))
    (emeld-refresh)))

(defun emeld--git-history-echo ()
  "Echo the current history position and the commit subject."
  (if (< emeld--git-history-index 0)
      (message "Working tree  (uncommitted changes vs %s)"
               (emeld--git-rev-label (car emeld--git-history)))
    (let ((subj (and emeld--git-right-commit
                     (plist-get emeld--git-right-commit :subject))))
      (message "Commit %d/%d  %s%s"
               (1+ emeld--git-history-index) (length emeld--git-history)
               (emeld--git-rev-label emeld--git-right-rev)
               (if subj (concat "  " subj) "")))))

(defun emeld--git-history-ensure ()
  "Ensure the buffer has a history list, building one on demand.
In git mode without an explicit history (e.g. after `emeld-git-diff'),
anchor a fresh first-parent walk at the current right revision when it is
a commit, otherwise at HEAD."
  (unless emeld--git-mode
    (user-error "Not comparing git revisions"))
  (unless emeld--git-history
    (let* ((anchor (if (stringp emeld--git-right-rev) emeld--git-right-rev "HEAD"))
           (h (emeld--git-history-list anchor emeld-git-history-limit)))
      (unless h (user-error "No commit history to step through"))
      (setq emeld--git-history h
            emeld--git-history-index 0))))

(defun emeld-git-history-older ()
  "Step to the previous (older) commit in the history walk.
Builds a walk anchored at the current commit if one is not already active."
  (interactive)
  (emeld--git-history-ensure)
  (if (>= (1+ emeld--git-history-index) (length emeld--git-history))
      (message "Oldest loaded commit (%d of %d; raise `emeld-git-history-limit' \
or re-run from an older commit)"
               (1+ emeld--git-history-index) (length emeld--git-history))
    (cl-incf emeld--git-history-index)
    (emeld--git-history-apply)
    (emeld--git-history-echo)))

(defun emeld-git-history-newer ()
  "Step to the next (newer) commit, back toward the starting revision.
Stepping newer once past the newest commit reaches the working-tree slot
\(uncommitted changes), available only when that commit is the current HEAD."
  (interactive)
  (emeld--git-history-ensure)
  (cond
   ((< emeld--git-history-index 0)
    (message "Working tree (newest)"))
   ((and (= emeld--git-history-index 0)
         (not (emeld--git-history-head-p)))
    (message "Newest commit (%s)" (emeld--git-rev-label (car emeld--git-history))))
   (t
    (cl-decf emeld--git-history-index)
    (emeld--git-history-apply)
    (emeld--git-history-echo))))

(defun emeld--parse-git-name-status (output)
  "Parse NUL-delimited `git diff --name-status -z' OUTPUT into entries.
Return a list of (RELPATH . STATUS) where STATUS is `different',
`left-only' (deleted in RIGHT) or `right-only' (added in RIGHT).  A rename
becomes the old path as `left-only' plus the new path as `right-only'; a
copy adds only the new path (the source is unchanged)."
  (let ((tokens (split-string output "\0" t))
        (entries nil))
    (while tokens
      (let ((code (car tokens)))
        (setq tokens (cdr tokens))
        (cond
         ((string-empty-p code) nil)
         ((eq (aref code 0) ?R)
          (let ((old (car tokens)) (new (cadr tokens)))
            (setq tokens (cddr tokens))
            (when old (push (cons old 'left-only) entries))
            (when new (push (cons new 'right-only) entries))))
         ((eq (aref code 0) ?C)
          (let ((new (cadr tokens)))
            (setq tokens (cddr tokens))
            (when new (push (cons new 'right-only) entries))))
         (t
          (let ((path (car tokens)))
            (setq tokens (cdr tokens))
            (when path
              (push (cons path (pcase (aref code 0)
                                 (?A 'right-only)
                                 (?D 'left-only)
                                 (_  'different)))
                    entries)))))))
    (nreverse entries)))

(defun emeld--start-git-diff (buf)
  "Run `git diff --name-status' for the rev pair and build the tree in BUF."
  (with-current-buffer buf
    (setq emeld--benchmark-start-time (float-time)
          ;; Cheap (`git show -s'); refreshed here so a swap or header
          ;; revision change updates the displayed commit context.
          emeld--git-left-commit (emeld--git-commit-info emeld--git-left-rev)
          emeld--git-right-commit (emeld--git-commit-info emeld--git-right-rev)
          ;; Cache HEAD so `emeld--git-rev-label' can mark a rev as HEAD
          ;; without a git call on every header-line redisplay.
          emeld--git-head-sha (emeld--git-resolve-head))
    ;; Refresh the pane-header labels now HEAD is known, so a rev that is
    ;; HEAD shows as "HEAD (SHA)" even when set by a caller before this scan.
    (when emeld--root-node
      (setf (emeld-node-name emeld--root-node)
            (emeld--git-rev-label emeld--git-left-rev)
            (emeld-node-name-right emeld--root-node)
            (emeld--git-rev-label emeld--git-right-rev))))
  (let* ((command (with-current-buffer buf (emeld--git-diff-command)))
         (chunks nil)
         (proc (make-process
                :name "emeld-git-diff"
                :buffer (generate-new-buffer " *emeld-git-diff*")
                :connection-type 'pipe
                :command command)))
    (set-process-filter
     proc
     (lambda (_p output) (push output chunks)))
    (set-process-sentinel
     proc
     (lambda (p event)
       (when (emeld--sentinel-done-p event)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let ((p-buf (process-buffer p)))
               (when (and p-buf (buffer-live-p p-buf)) (kill-buffer p-buf)))
             (emeld--build-git-tree buf (apply #'concat (nreverse chunks))))))))
    (setq emeld--scan-process proc)))

(defun emeld--build-git-tree (buf output)
  "Build the node tree in BUF from git name-status OUTPUT.
Parses OUTPUT into entries and feeds them through the shared progressive
tree builder; the `emeld--git-mode' guards keep that builder from probing
the filesystem for paths that exist only in history."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((root emeld--root-node)
             (entries (emeld--parse-git-name-status output))
             (scan-gen emeld--scan-generation)
             (old-gc gc-cons-threshold))
        (setq gc-cons-threshold (* 128 1024 1024))
        (setq emeld--status-phase 'building-tree
              emeld--scan-total (length entries)
              emeld--scan-processed (length entries)
              emeld--last-render-time (float-time))
        (emeld--update-header-line)
        (emeld--bench-push "git diff done")
        (if (/= scan-gen emeld--scan-generation)
            (setq gc-cons-threshold old-gc)
          (emeld--entries-to-nodes-progressive
           entries "" root buf
           (lambda (top-level-children)
             (setf (emeld-node-children root) top-level-children)
             (emeld--render))
           (lambda ()
             (with-current-buffer buf
               (setf (emeld-node-diff-status root)
                     (if (cl-some (lambda (c) (not (eq (emeld-node-diff-status c) 'same)))
                                  (emeld-node-children root))
                         'different 'same))
               (setq emeld--scan-process nil
                     emeld--scan-buf nil)
               (if emeld--git-expand-changes-pending
                   (progn
                     (emeld--set-expanded-changes)
                     (setq emeld--git-expand-changes-pending nil))
                 (emeld--restore-expanded root))
               (unless emeld-show-same
                 (setq emeld--status-phase 'done))
               (emeld--render)
               (emeld--bench-push "tree built")
               (emeld--bench-report)
               (setq gc-cons-threshold old-gc)
               (if emeld-show-same
                   (emeld--scan-same-async buf)
                 (emeld--update-header-line))))
           scan-gen))))))

(defun emeld--git-blob-buffer (rev relpath)
  "Return a buffer holding RELPATH's content at REV in the current repo.
For the `working' side the on-disk file is visited.  For a revision or the
index (`staged') `git show' is run into a read-only buffer, with the file's
normal major mode enabled so the content is fontified."
  (cond
   ((eq rev 'working)
    (find-file-noselect (expand-file-name relpath emeld--git-repo)))
   (t
    (let* ((spec (if (eq rev 'staged) (concat ":" relpath)
                   (concat rev ":" relpath)))
           (repo emeld--git-repo)
           (buf (get-buffer-create
                 (format "*emeld-git %s:%s*" (emeld--git-rev-label rev) relpath))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (let ((default-directory repo))
            (process-file "git" nil t nil "show" spec))
          (goto-char (point-min))
          (setq default-directory repo)
          ;; Bind a fake file name so `set-auto-mode' picks the right mode.
          (let ((buffer-file-name (expand-file-name relpath repo)))
            (ignore-errors (set-auto-mode)))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)))
      buf))))

(defun emeld--git-diff-show (relpath what)
  "Display a `git diff' of RELPATH (or the whole tree if RELPATH is nil).
Uses the buffer's revision pair and shows the result in a `diff-mode'
buffer.  WHAT labels the echo-area message."
  (let* ((repo emeld--git-repo)
         (revargs (emeld--git-rev-args emeld--git-left-rev emeld--git-right-rev))
         (args (append (list "diff" "--no-color") (emeld--whitespace-args) revargs
                       (and relpath (list "--" relpath))))
         (buf (get-buffer-create "*emeld-git-diff*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let ((default-directory repo))
          (apply #'process-file "git" nil t nil args))
        (goto-char (point-min))
        (setq default-directory repo)
        (diff-mode)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)))
    (if (with-current-buffer buf (= (point-min) (point-max)))
        (message "No diff for %s" what)
      (display-buffer buf)
      (message "Diff of %s" what))))

(defun emeld--git-commit-cell-lines (info expanded)
  "Return the display lines for one commit column, or nil when INFO is nil.
The first line is the fold arrow, abbreviated SHA and subject; when EXPANDED
the author, date and body lines follow.  Lines are propertized; the caller
word-wraps each to the column width."
  (when info
    (let ((lines (list (concat (propertize (if expanded "▾ " "▸ ") 'face 'bold)
                               (propertize (concat (plist-get info :sha) "  ")
                                           'face 'font-lock-constant-face)
                               (propertize (plist-get info :subject)
                                           'face 'emeld-git-commit-subject-face)))))
      (when expanded
        (push (propertize (concat "  " (plist-get info :author))
                          'face 'emeld-git-commit-meta-face)
              lines)
        (push (propertize (concat "  " (plist-get info :date))
                          'face 'emeld-git-commit-meta-face)
              lines)
        (let ((body (plist-get info :body)))
          (unless (string-empty-p body)
            (push "" lines)
            (dolist (bl (split-string body "\n"))
              (push (concat "  " bl) lines)))))
      (nreverse lines))))

(defun emeld--git-wrap-line (str width indent)
  "Word-wrap STR to WIDTH display columns, returning a list of physical lines.
Continuation lines (after the first) carry INDENT leading spaces.  Wraps on
whitespace where possible, hard-breaking words longer than the available
width.  Text properties on STR are preserved."
  (if (or (<= width 0) (<= (string-width str) width))
      (list str)
    (let* ((pad (make-string indent ?\s))
           (lines nil)
           (i 0)
           (n (length str))
           (seg-start 0)
           (cur-width 0)
           (limit width)
           (last-space -1))
      (while (< i n)
        (let* ((ch (aref str i))
               (cw (char-width ch)))
          (when (eq ch ?\s)
            (setq last-space i))
          (if (and (> (+ cur-width cw) limit) (> i seg-start))
              (let ((break (if (> last-space seg-start) last-space i)))
                (push (concat (and lines pad) (substring str seg-start break)) lines)
                (setq seg-start (if (and (> last-space seg-start)
                                         (eq (aref str last-space) ?\s))
                                    (1+ break) break)
                      i seg-start
                      cur-width 0
                      last-space -1
                      limit (- width indent)))
            (setq cur-width (+ cur-width cw)
                  i (1+ i)))))
      (when (< seg-start n)
        (push (concat (and lines pad) (substring str seg-start n)) lines))
      (nreverse lines))))

(defun emeld--insert-git-commit-block ()
  "Insert the foldable git commit block at point, in two columns (git mode).
The left commit sits under the left (old) header and the right commit under
the right (new) header, matching the pane split.  Folded shows just each
commit's SHA and subject; `emeld--git-commit-expanded' adds the author, date
and full body.  The block carries the `emeld-git-commit' text property so RET
\(or `c') toggles the fold."
  (when (or emeld--git-left-commit emeld--git-right-commit)
    (let* ((cw emeld--column-width)
           (left (emeld--git-commit-cell-lines emeld--git-left-commit
                                               emeld--git-commit-expanded))
           (right (emeld--git-commit-cell-lines emeld--git-right-commit
                                                emeld--git-commit-expanded))
           (rows (max (length left) (length right)))
           (start (point)))
      (dotimes (i rows)
        ;; Word-wrap each cell to the column width so nothing is clipped, then
        ;; emit the wrapped sub-lines side by side.  Wrapping both columns per
        ;; logical line keeps each commit's line vertically grouped with its
        ;; counterpart; the left cell is padded to the column width so the
        ;; right cell always starts at the pane boundary.
        (let* ((lw (emeld--git-wrap-line (or (nth i left) "") cw 2))
               (rw (emeld--git-wrap-line (or (nth i right) "") cw 2))
               (sub (max (length lw) (length rw))))
          (dotimes (j sub)
            (insert (truncate-string-to-width (or (nth j lw) "") cw nil ?\s))
            (insert (or (nth j rw) ""))
            (insert "\n"))))
      (add-text-properties start (point)
                           '(emeld-git-commit t mouse-face highlight)))))

(defface emeld-git-commit-subject-face
  '((t :inherit default :weight bold))
  "Face for a commit's subject line in the git commit-message block."
  :group 'emeld)

(defface emeld-git-commit-meta-face
  '((t :inherit font-lock-comment-face))
  "Face for commit author/date metadata in the git commit-message block."
  :group 'emeld)

(defun emeld-git-toggle-commit-message ()
  "Fold or unfold the git commit-message block (git mode only).
Folded shows just each commit's subject line; unfolded adds the author,
date and full body.  Also reachable with RET on the block."
  (interactive)
  (unless emeld--git-mode
    (user-error "Commit messages are only shown when comparing git revisions"))
  (unless (or emeld--git-left-commit emeld--git-right-commit)
    (user-error "Neither side is a commit (working tree / index)"))
  (setq emeld--git-commit-expanded (not emeld--git-commit-expanded))
  (emeld--render)
  (message "Commit message: %s" (if emeld--git-commit-expanded "expanded" "folded")))

(defun emeld--git-action (node on-left)
  "RET action for NODE in git mode; ON-LEFT means point is in the left column.
Directories toggle expansion; a modified file ediffs its two revisions; an
added/deleted/identical file is shown at the appropriate revision."
  (cond
   ((emeld-node-is-dir node) (emeld-toggle-expand))
   (t
    (let ((rel (emeld--node-relpath node))
          (left (emeld-node-left-path node))
          (right (emeld-node-right-path node)))
      (cond
       ((and left right (eq (emeld-node-diff-status node) 'different))
        (ediff-buffers (emeld--git-blob-buffer emeld--git-left-rev rel)
                       (emeld--git-blob-buffer emeld--git-right-rev rel)))
       ((or left right)
        (let ((rev (cond ((and on-left left) emeld--git-left-rev)
                         ((and (not on-left) right) emeld--git-right-rev)
                         (left emeld--git-left-rev)
                         (t emeld--git-right-rev))))
          (switch-to-buffer-other-window (emeld--git-blob-buffer rev rel))))
       (t (message "No file on this side")))))))

(defun emeld--git-soft-diff (node)
  "Preview NODE in git mode for `emeld-soft-diff' (SPC) without selecting.
A modified file shows a `git diff'; an added/deleted/identical file is
displayed at its revision in another window."
  (let ((rel (emeld--node-relpath node))
        (left (emeld-node-left-path node))
        (right (emeld-node-right-path node)))
    (cond
     ((and left right (eq (emeld-node-diff-status node) 'different))
      (emeld--git-diff-show rel (emeld-node-name node)))
     ((or left right)
      (let ((rev (if right emeld--git-right-rev emeld--git-left-rev)))
        (save-selected-window
          (display-buffer (emeld--git-blob-buffer rev rel)))))
     (t (message "No file at point")))))


(provide 'emeld-git)
;;; emeld-git.el ends here
