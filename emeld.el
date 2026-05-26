;;; emeld.el --- Meld-Inspired Local Diff -*- lexical-binding: t; -*-

;; Keywords: files, tools
;; Version: 0.18.0

;;; Commentary:
;; A simple side-by-side directory comparison tool inspired by Meld.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'subr-x)
(require 'transient)

(declare-function find-library-name "find-func" (library))
(defvar savehist-additional-variables)

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

(defcustom emeld-respect-gitignore t
  "When non-nil, respect `.gitignore` rules during directory comparison.
Simple patterns from each root `.gitignore' are passed to `diff
--exclude' to skip ignored files at the OS level.  Anything that slips
past that (anchored, nested or otherwise complex rules) is caught during
tree assembly by a single cached `git ls-files' query per root, which
also honors `.git/info/exclude' and the global excludes file.
Requires the `git' executable; has no effect outside a work tree."
  :type 'boolean
  :group 'emeld)

(defvar emeld-simple-comparison nil
  "Obsolete.  Removed in 0.9.4.  Kept for dir-locals compatibility.")
(make-obsolete-variable 'emeld-simple-comparison nil "0.9.4")

(defcustom emeld-presets nil
  "Saved directory pairs for quick recall, as a list of (NAME LEFT RIGHT).
Populated by `emeld-save-preset' and recalled by `emeld-load-preset', so
frequently-compared directory pairs can be re-run without retyping their
paths.  Registered with `savehist-additional-variables' (see below) so
presets persist across sessions when `savehist-mode' is enabled."
  :type '(alist :key-type (string :tag "Name")
                :value-type (group (directory :tag "Left")
                                   (directory :tag "Right")))
  :group 'emeld)

;; Persist presets across sessions via savehist, when it is in use.
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'emeld-presets))

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
(defvar-local emeld--chunk-worker-timer nil
  "Holds the active chunking timer to prevent overlapping timer execution.")
(defvar-local emeld--scan-generation 0
  "Incremented on each new scan start. Workers check this to abort stale operations.")
(defvar-local emeld--active-filter-groups nil
  "List of currently active filter group names.")
(defvar-local emeld--left-root nil)
(defvar-local emeld--right-root nil)
(defvar-local emeld--root-node nil)
(defvar-local emeld--column-width nil)
(defvar-local emeld--saved-expanded nil
  "Saved expanded directory paths to restore after refresh.")
(defcustom emeld-benchmark nil
  "When non-nil, record and display detailed timing for each pipeline phase."
  :type 'boolean
  :group 'emeld)

(defcustom emeld-benchmark-file
  (expand-file-name "BENCHMARKS.org"
                    (or (and load-file-name (file-name-directory load-file-name))
                        user-emacs-directory))
  "Org file where `emeld-benchmark-record' appends timing entries.
Defaults to BENCHMARKS.org beside emeld.el so the log can be kept under
version control.  Each entry stamps the machine specs and emeld build
\(`emeld--system-info') so readings can be compared across commits and
machines to catch regressions."
  :type 'file
  :group 'emeld)

(defvar-local emeld--benchmark-data nil
  "List of (LABEL . FLOAT-TIME) timestamps for the current scan cycle.")
(defvar-local emeld--benchmark-start-time nil
  "Float-time captured when the background diff process starts.")
(defvar emeld--benchmark-log nil
  "Session list of completed benchmark runs awaiting `emeld-benchmark-record'.
Newest first.  Each element is a plist with :time :kind :left :right
:entries :total and :stages (a list of (LABEL STAGE-MS CUM-MS)), appended
by `emeld--bench-report' and flushed to `emeld-benchmark-file'.")

(defvar-local emeld--assembly-done nil
  "Non-nil when the progressive tree assembly has fully completed.")

(defvar-local emeld--status-phase 'idle
  "Current phase of the emeld background processing cycle.
Values can be 'idle, 'running-diff, 'building-tree, or 'done.")

(defvar-local emeld--scan-diff-count 0
  "Count of `diff' output lines received so far during the running scan.
Surfaced as a live entry tally in the header line.")
(defvar-local emeld--scan-processed 0
  "Number of diff lines parsed so far while building the tree.")
(defvar-local emeld--scan-total 0
  "Total diff lines to parse while building the tree, or 0 until known.
With `emeld--scan-processed' this drives the header-line progress bar.")

(defvar-local emeld--saved-point-path nil
  "Saved point path to restore across async refresh cycle.")
(defvar-local emeld--saved-point-col nil
  "Saved point column to restore across async refresh cycle.")
(defvar-local emeld--marked-paths nil
  "Hash table of relative paths for marked nodes (Dired-style marking).")
(defvar-local emeld--last-render-time nil
  "Float-time of the last `emeld--render' call, for render throttling.")
(defvar-local emeld--same-skip-files nil
  "Hash set of relpaths already in the tree, for the `list' same-file merge.
Files here (differing, left-only, or already merged) are not re-added.")
(defvar-local emeld--same-left-only-dirs nil
  "Hash set of left-only directory relpaths, for the `list' same-file merge.
Any enumerated left file under one of these is part of a left-only subtree,
not an identical file, so it is skipped.")
(defvar-local emeld--gitignore-cache nil
  "Memoized git-ignored path sets, keyed by absolute root directory.
Each value is a hash set of ignored relative paths (directories stored
without their trailing slash), or the symbol `none' (not a git work
tree) or `pending' (async `git ls-files' still running).  Reset on each
scan by `emeld-refresh' so edits to `.gitignore' are picked up.")
(defvar-local emeld--gitignore-procs nil
  "Live async `git ls-files' processes pre-warming `emeld--gitignore-cache'.
Killed by `emeld--stop-scan' so a rescan cannot leave stale workers.")

(defsubst emeld--bench-push (label)
  "Record a timestamp LABEL in `emeld--benchmark-data' when benchmarking."
  (when emeld-benchmark
    (push (cons label (float-time)) emeld--benchmark-data)))

(defun emeld--bench-report ()
  "Print a structured timing report from `emeld--benchmark-data'.
Also push a structured copy of the run onto `emeld--benchmark-log' so
`emeld-benchmark-record' can later write it to `emeld-benchmark-file'."
  (when emeld-benchmark
    (let* ((data (nreverse emeld--benchmark-data))
           (base (or emeld--benchmark-start-time (cdar data)))
           (last-time (cdr (car (last data))))
           (total (* (- last-time base) 1000.0))
           (pairs nil)
           (stages nil)
           (prev-time base))
      (dolist (item data)
        (let* ((label (car item))
               (timestamp (cdr item))
               (stage-delta (* (- timestamp prev-time) 1000.0))
               (cum-delta (* (- timestamp base) 1000.0)))
          (push (format "  %-30s %5.0f ms (cumulative %6.0f ms)"
                        label stage-delta cum-delta)
                pairs)
          (push (list label (round stage-delta) (round cum-delta)) stages)
          (setq prev-time timestamp)))
      (setq stages (nreverse stages))
      (push (list :time (current-time)
                  :kind (if (and stages (string-prefix-p "same" (car (car stages))))
                            'same 'initial)
                  :left emeld--left-root
                  :right emeld--right-root
                  :entries emeld--scan-total
                  :total (round total)
                  :stages stages)
            emeld--benchmark-log)
      (setq emeld--benchmark-data nil
            emeld--benchmark-start-time nil)
      (message "=== Emeld Benchmark ===\n%s\n  %-30s %5.0f ms (total)\n========================"
               (string-join (nreverse pairs) "\n")
               "elapsed total"
               total))))

(defun emeld--package-version ()
  "Return the emeld version from its source header, or \"?\"."
  (or (ignore-errors
        (require 'find-func)
        (with-temp-buffer
          (insert-file-contents (find-library-name "emeld") nil 0 4000)
          (goto-char (point-min))
          (when (re-search-forward "^;;+[ \t]*Version:[ \t]*\\(.+\\)$" nil t)
            (string-trim (match-string 1)))))
      "?"))

(defun emeld--git-describe ()
  "Return \"<short-rev>[+dirty]\" for the emeld working tree, or \"?\"."
  (or (ignore-errors
        (require 'find-func)
        (let ((default-directory (file-name-directory (find-library-name "emeld"))))
          (when (and (executable-find "git")
                     (eq 0 (call-process "git" nil nil nil
                                         "rev-parse" "--is-inside-work-tree")))
            (let ((rev (with-temp-buffer
                         (when (eq 0 (call-process "git" nil t nil
                                                   "rev-parse" "--short" "HEAD"))
                           (string-trim (buffer-string)))))
                  (dirty (with-temp-buffer
                           (call-process "git" nil t nil "status" "--porcelain")
                           (> (buffer-size) 0))))
              (and rev (concat rev (if dirty "+dirty" "")))))))
      "?"))

(defun emeld--system-info ()
  "Return an alist of machine and build facts for a benchmark record.
CPU and memory are read from /proc on Linux; other systems fall back to
\"unknown\" for those fields."
  (let ((cpu "unknown")
        (cores (if (fboundp 'num-processors) (num-processors) nil))
        (mem "unknown"))
    (when (file-readable-p "/proc/cpuinfo")
      (with-temp-buffer
        (insert-file-contents "/proc/cpuinfo")
        (goto-char (point-min))
        (when (re-search-forward "^model name[ \t]*:[ \t]*\\(.+\\)$" nil t)
          (setq cpu (string-trim (match-string 1))))
        (unless cores
          (goto-char (point-min))
          (setq cores (how-many "^processor[ \t]*:")))))
    (when (file-readable-p "/proc/meminfo")
      (with-temp-buffer
        (insert-file-contents "/proc/meminfo")
        (goto-char (point-min))
        (when (re-search-forward "^MemTotal:[ \t]*\\([0-9]+\\) kB" nil t)
          (setq mem (format "%.1f GiB"
                            (/ (string-to-number (match-string 1)) 1048576.0))))))
    (list (cons "Host"      (system-name))
          (cons "OS"        (string-trim (shell-command-to-string "uname -srm")))
          (cons "CPU"       (if cores (format "%s (%d threads)" cpu cores) cpu))
          (cons "Memory"    mem)
          (cons "Emacs"     emacs-version)
          (cons "Emeld"     (emeld--package-version))
          (cons "Emeld rev" (emeld--git-describe)))))

(defun emeld--org-table (header rows)
  "Return an aligned Org table string for HEADER and ROWS (lists of cells).
Cells are coerced to strings; columns are padded to their widest cell."
  (let* ((to-str (lambda (r) (mapcar (lambda (c) (format "%s" c)) r)))
         (h (funcall to-str header))
         (rs (mapcar to-str rows))
         (ncol (length h))
         (widths (mapcar (lambda (c)
                           (apply #'max (mapcar (lambda (r) (length (nth c r)))
                                                (cons h rs))))
                         (number-sequence 0 (1- ncol))))
         (fmt-row (lambda (r)
                    (concat "| "
                            (mapconcat (lambda (c)
                                         (format (format "%%-%ds" (nth c widths))
                                                 (nth c r)))
                                       (number-sequence 0 (1- ncol)) " | ")
                            " |")))
         (hline (concat "|" (mapconcat (lambda (w) (make-string (+ w 2) ?-))
                                       widths "+")
                        "|")))
    (concat (funcall fmt-row h) "\n" hline "\n"
            (mapconcat fmt-row rs "\n") "\n")))

(defun emeld--benchmark-run-org (run)
  "Format one RUN plist (see `emeld--benchmark-log') as an Org subtree."
  (let* ((kind (plist-get run :kind))
         (entries (plist-get run :entries))
         (rows (append (mapcar (lambda (s) (list (nth 0 s) (nth 1 s) (nth 2 s)))
                               (plist-get run :stages))
                       (list (list "*total*" (plist-get run :total) "")))))
    (concat
     (format "*** %s :: %s vs %s%s\n"
             (if (eq kind 'same) "identical-files (S)" "initial (diff -rq)")
             (abbreviate-file-name (or (plist-get run :left) "?"))
             (abbreviate-file-name (or (plist-get run :right) "?"))
             (if (and entries (> entries 0)) (format " — %d entries" entries) ""))
     (emeld--org-table '("stage" "ms" "cumulative ms") rows))))

(defun emeld--benchmark-org-entry (runs info time)
  "Build the Org entry string for RUNS, system INFO, recorded at TIME."
  (concat
   (format "* %s — emeld %s (%s)\n"
           (format-time-string "[%Y-%m-%d %a %H:%M]" time)
           (cdr (assoc "Emeld" info)) (cdr (assoc "Emeld rev" info)))
   "** System\n"
   (mapconcat (lambda (kv) (format "- %s :: %s" (car kv) (cdr kv))) info "\n")
   "\n** Runs\n"
   (mapconcat #'emeld--benchmark-run-org runs "\n")))

(defun emeld-benchmark-record ()
  "Append the benchmark runs collected this session to `emeld-benchmark-file'.
Runs accumulate (one per completed scan) while `emeld-benchmark' is
non-nil.  This writes them under a single Org heading stamped with the
machine specs and emeld build from `emeld--system-info', then clears the
collected runs and visits the file at the new entry."
  (interactive)
  (unless emeld--benchmark-log
    (user-error "No benchmark runs collected; enable `emeld-benchmark' and run a comparison"))
  (let* ((runs (reverse emeld--benchmark-log))
         (file (expand-file-name emeld-benchmark-file))
         (new (not (file-exists-p file)))
         (entry (emeld--benchmark-org-entry runs (emeld--system-info) (current-time))))
    (with-temp-buffer
      (when new
        (insert "#+TITLE: Emeld Benchmark Log\n"
                "#+STARTUP: showall\n\n"))
      (insert entry "\n")
      (write-region (point-min) (point-max) file t 'silent))
    (setq emeld--benchmark-log nil)
    ;; In batch (bench.sh) just write; interactively, visit the new entry.
    (unless noninteractive
      (find-file file)
      (goto-char (point-max)))
    (message "Recorded %d benchmark run(s) to %s"
             (length runs) (abbreviate-file-name file))))

(defun emeld-benchmark-system-info ()
  "Insert this machine's specs and emeld build (`emeld--system-info') at point.
Useful for filling in the System block of a hand-written benchmark entry."
  (interactive)
  (insert (mapconcat (lambda (kv) (format "- %s :: %s" (car kv) (cdr kv)))
                     (emeld--system-info) "\n")
          "\n"))

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
Only sets the path for the side(s) where the file exists.
Single-sided directories are populated and expanded eagerly so that
all changed files are visible on initial scan."
  (let ((name (file-name-nondirectory (directory-file-name relpath))))
    (if (memq status '(left-only right-only))
        (let* ((side-root (if (eq status 'left-only) emeld--left-root emeld--right-root))
               (side-path (and side-root (concat (file-name-as-directory side-root) relpath))))
          (if (and side-path (file-directory-p side-path))
              (let ((dir-node (emeld-node-create
                               :name name
                               :left-path (and (eq status 'left-only) side-path)
                               :right-path (and (eq status 'right-only) side-path)
                               :parent parent
                               :is-dir t
                               :expanded t
                               :children nil
                               :diff-status status)))
                (emeld--maybe-populate-lazy-dir dir-node)
                dir-node)
            (emeld-node-create
             :name name
             :left-path (and (eq status 'left-only) side-path)
             :right-path (and (eq status 'right-only) side-path)
             :parent parent
             :diff-status status)))
      (emeld-node-create
       :name name
       :left-path (and (memq status '(same different left-only))
                       emeld--left-root
                       (concat (file-name-as-directory emeld--left-root) relpath))
       :right-path (and (memq status '(same different right-only))
                        emeld--right-root
                        (concat (file-name-as-directory emeld--right-root) relpath))
       :parent parent
       :diff-status status))))

(defun emeld--entries-to-nodes (entries prefix parent)
  "Convert ENTRIES (alist of (REL-PATH . STATUS)) into emeld-node tree.
Directories are grouped first, followed by files, each sorted alphabetically."
  (let ((dirs (make-hash-table :test 'equal))
        (files nil))
    (dolist (entry entries)
      (let* ((raw-path (car entry))
             (status (cdr entry))
             (path (if (string-prefix-p "/" raw-path)
                       (substring raw-path 1)
                     raw-path))
             (name (file-name-nondirectory (directory-file-name path)))
             (filt (emeld--filtered-p name path)))
        (unless (or (string-empty-p path) filt)
          (if (string-match "/" path)
              (let* ((slash-pos (match-beginning 0))
                     (dir-name (substring path 0 slash-pos))
                     (rest (substring path (1+ slash-pos))))
                (if (string-empty-p dir-name)
                    (push (cons rest status) files)
                  (push (cons rest status)
                        (gethash dir-name dirs))))
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
                                 :left-path (and emeld--left-root
                                                 (concat (file-name-as-directory emeld--left-root) sub-prefix))
                                 :right-path (and emeld--right-root
                                                  (concat (file-name-as-directory emeld--right-root) sub-prefix))
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
  "Expand single-sided directory entries to include all their recursive children cleanly."
  (let ((expanded nil)
        (left-base (expand-file-name emeld--left-root))
        (right-base (expand-file-name emeld--right-root)))
    (dolist (entry entries)
      (let ((path (car entry))
            (status (cdr entry)))
        (cond
         ((memq status '(left-only right-only))
          (let* ((is-left (eq status 'left-only))
                 (base-dir (expand-file-name path (if is-left left-base right-base))))
            (if (file-directory-p base-dir)
                (let ((sub-files (directory-files-recursively base-dir "" t)))
                  (push entry expanded)
                  (dolist (f sub-files)
                    (let* ((abs-f (expand-file-name f))
                           (match-base (if is-left left-base right-base))
                           (rel (if (string-prefix-p match-base abs-f)
                                    (substring abs-f (length match-base))
                                  abs-f)))
                      (when (string-prefix-p "/" rel)
                        (setq rel (substring rel 1)))
                      (unless (string-empty-p rel)
                        (push (cons rel status) expanded)))))
              (push entry expanded))))
         (t
          (push entry expanded)))))
    (nreverse expanded)))

(defcustom emeld-batch-size 2000
  "Number of diff lines to parse per chunk during tree assembly.
Lower numbers keep Emacs snappier; higher numbers process faster."
  :type 'integer
  :group 'emeld)

(defcustom emeld-tree-batch-size 5000
  "Number of entries or nodes to process per time-slice during tree conversion."
  :type 'integer
  :group 'emeld)

(defcustom emeld-render-throttle 0.15
  "Minimum seconds between intermediate buffer redraws during a scan.
Each redraw rebuilds the whole visible tree, so throttling keeps Emacs
responsive while a large comparison streams in.  The final result is
always rendered once the scan completes regardless of this value."
  :type 'number
  :group 'emeld)

(defcustom emeld-same-files-method 'list
  "How the show-same (S) merge finds identical files.

`list' (default, fast): list the left side's files and treat as identical
every file that the initial scan did not already report as differing or
left-only.  This reuses the initial `diff -rq' content comparison instead
of repeating it, so it is dramatically faster on large trees.

`diff' (exact): re-run `diff -rqs', which re-reads and compares the
content of every file.  Slower, but does not rely on enumerating the file
list — use it if the `list' result ever looks wrong."
  :type '(choice (const :tag "List left files and subtract (fast)" list)
                 (const :tag "Re-run diff -rqs (exact, slow)" diff))
  :group 'emeld)

(defcustom emeld-list-command '("find" "." "-type" "f")
  "Command (program then args) that lists files for `emeld-same-files-method'.
Run with `default-directory' bound to the directory being listed; it must
print one path per line relative to that directory (a leading \"./\" is
stripped).  Defaults to `find', which is universal; set it to e.g.
\\='(\"fd\" \"--type\" \"f\" \"--hidden\" \"--no-ignore\") to use fd."
  :type '(repeat string)
  :group 'emeld)

(defun emeld--build-tree-from-diff (buf)
  "Build tree from `diff -rq' output asynchronously using chunked iterations."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((left emeld--left-root)
             (right emeld--right-root)
             (root emeld--root-node)
             (remaining-lines emeld--scan-diff-lines)
             (total-lines (length remaining-lines))
             (entries nil)
             (processed-count 0)
             (old-gc gc-cons-threshold))
        
        (setq emeld--scan-diff-lines nil)
        (setq gc-cons-threshold (* 128 1024 1024))
        (setq emeld--status-phase 'building-tree)
        (setq emeld--scan-total total-lines
              emeld--scan-processed 0)
        (setq emeld--last-render-time (float-time))
        (emeld--update-header-line)
        (emeld--bench-push "diff done")
        
        (cl-labels
            ((process-lines-chunk
               ()
               (if (not (buffer-live-p buf))
                   (setq gc-cons-threshold old-gc)
                 (with-current-buffer buf
                    (if (input-pending-p)
                        (run-with-timer 0.001 nil #'process-lines-chunk)
                      (let ((chunk-count 0))
                        (while (and remaining-lines (< chunk-count emeld-batch-size))
                          (let ((parsed (emeld--parse-diff-line (car remaining-lines) left right)))
                            (when parsed
                              (push parsed entries)))
                          (setq remaining-lines (cdr remaining-lines))
                          (setq chunk-count (1+ chunk-count))
                          (setq processed-count (1+ processed-count)))
                        (setq emeld--scan-processed processed-count)
                        (force-mode-line-update)

                        (if remaining-lines
                            (run-with-timer 0.001 nil #'process-lines-chunk)
                           (emeld--bench-push "parse done")
                          
                           (message "Structuring tree nodes progressively...")
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
                              (emeld--restore-expanded root)
                              (emeld--render)
                              (emeld--bench-push "tree built")
                              (emeld--bench-report)
                              (setq gc-cons-threshold old-gc)
                              (if emeld-show-same
                                  ;; Differences are placed; merge identical
                                  ;; files via the fast path rather than having
                                  ;; assembled them all from diff -rqs.
                                  (emeld--scan-same-async buf)
                                (setq emeld--status-phase 'done)
                                (emeld--update-header-line))))))))))))
          (run-with-timer 0.001 nil #'process-lines-chunk))))))

(defun emeld--entries-to-nodes-progressive (entries prefix parent buf initial-render-cb final-cb &optional gen)
  "Convert ENTRIES into nodes progressively.
Prioritizes branches containing modifications or differences so they render first,
allowing identical/same branches to populate quietly afterward."
  (let ((dirs (make-hash-table :test 'equal))
        (files nil)
        (dir-names (make-hash-table :test 'equal))
        (file-nodes nil)
        (current-gen (or gen (with-current-buffer buf emeld--scan-generation))))
    
    ;; 1. Parse flat entries into immediate files and directory buckets
    (let ((tail entries))
      (while tail
        (let ((entry (car tail)))
          (let* ((raw-path (car entry))
                 (status (cdr entry))
                 (path (cond ((string-prefix-p "./" raw-path) (substring raw-path 2))
                             ((string-prefix-p "/" raw-path) (substring raw-path 1))
                             (t raw-path)))
                 (name (file-name-nondirectory (directory-file-name path))))
            (unless (or (string-empty-p path) (emeld--filtered-p name path))
              (if (string-match "/" path)
                  (let* ((slash-pos (match-beginning 0))
                         (dir-name (substring path 0 slash-pos))
                         (rest (substring path (1+ slash-pos))))
                    (if (string-empty-p dir-name)
                        (push (cons rest status) files)
                      (puthash dir-name
                               (cons (cons rest status) (gethash dir-name dirs))
                               dirs)))
                (push (cons path status) files)))))
        (setq tail (cdr tail))))

    ;; Record immediate subdirectory keys to isolate floating files
    (maphash (lambda (k _) (puthash k t dir-names)) dirs)

    ;; 2. Instantiate file node items for the current tier
    (let ((tail files))
      (while tail
        (let ((entry (car tail)))
          (let* ((path (car entry))
                 (status (cdr entry)))
            (unless (or (string-empty-p path) (gethash path dir-names))
              (let ((sub-rel (if (string-empty-p prefix) path (concat prefix "/" path))))
                (push (emeld--make-file-node sub-rel parent status) file-nodes)))))
        (setq tail (cdr tail))))

    ;; 3. Check if this is an implicit matching node branch that needs physical unrolling
    (when (and (null files) (hash-table-empty-p dirs) (not (string-empty-p prefix)))
      (with-current-buffer buf
        (let* ((left-root emeld--left-root)
               (right-root emeld--right-root)
               (abs-left (concat (file-name-as-directory left-root) prefix))
               (abs-right (concat (file-name-as-directory right-root) prefix))
               (target-dir (cond ((file-directory-p abs-left) abs-left)
                                 ((file-directory-p abs-right) abs-right))))
          (when (and target-dir (file-directory-p target-dir))
            (let ((items (directory-files target-dir nil "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\)")))
              (dolist (item items)
                (let* ((item-rel (concat prefix "/" item))
                       (item-abs-left (concat (file-name-as-directory left-root) item-rel))
                       (item-abs-right (concat (file-name-as-directory right-root) item-rel))
                       (is-dir (or (file-directory-p item-abs-left) (file-directory-p item-abs-right))))
                  (if is-dir
                      (puthash item (list (cons "" 'same)) dirs)
                    (push (cons item 'same) files)))))))))

    ;; Re-sync hash indicators if physical backfill triggered
    (maphash (lambda (k _) (puthash k t dir-names)) dirs)
    
    (let ((tail files))
      (while tail
        (let ((entry (car tail)))
          (let* ((path (car entry))
                 (status (cdr entry)))
            (unless (or (string-empty-p path) (gethash path dir-names)
                        (cl-find-if (lambda (n) (string= (emeld-node-name n) path)) file-nodes))
              (let ((sub-rel (if (string-empty-p prefix) path (concat prefix "/" path))))
                (push (emeld--make-file-node sub-rel parent status) file-nodes)))))
        (setq tail (cdr tail))))

    ;; 4. Build immediate subdirectory structures
    (let ((dir-nodes nil)
          (priority-queue nil)
          (same-queue nil))
      (maphash
       (lambda (dir-name sub-entries)
         (let ((sub-prefix (if (string-empty-p prefix) dir-name (concat prefix "/" dir-name))))
           (unless (or (string-empty-p dir-name) (string= dir-name "'") (emeld--filtered-p dir-name sub-prefix))
             (let* ((only-same (cl-every (lambda (e) (eq (cdr e) 'same)) sub-entries))
                    (should-expand t)
                    (dir-node (emeld-node-create
                               :name dir-name
                               :left-path (and emeld--left-root 
                                               (not (string-empty-p sub-prefix))
                                               (concat (file-name-as-directory emeld--left-root) sub-prefix))
                               :right-path (and emeld--right-root 
                                                (not (string-empty-p sub-prefix))
                                                (concat (file-name-as-directory emeld--right-root) sub-prefix))
                               :parent parent
                               :is-dir t
                               :expanded should-expand
                               :diff-status (if only-same 'same 'different)))
                    (task-item (list sub-entries sub-prefix dir-node)))
               (push dir-node dir-nodes)
               (if only-same
                   (push task-item same-queue)
                 (push task-item priority-queue))))))
       dirs)

    (let ((current-tier-nodes (sort (nconc file-nodes dir-nodes)
                                    (lambda (a b)
                                      (let ((a-dir (emeld-node-is-dir a))
                                            (b-dir (emeld-node-is-dir b)))
                                        (if (eq a-dir b-dir)
                                            (string< (emeld-node-name a) (emeld-node-name b))
                                          a-dir))))))
      (funcall initial-render-cb current-tier-nodes)

      ;; 5. Time-sliced background processing loop.
      ;; final-cb must wait for sub-chains: a level's own queues drain as soon
      ;; as sub-dirs are *dispatched*, but their sub-trees are still being
      ;; built on later timer ticks.  Track outstanding sub-chains so the
      ;; final-cb only fires when this whole subtree is done.
      (let ((outstanding 0)
            (own-queues-done nil))
        (letrec
            ((finish-check
              (lambda ()
                (when (and own-queues-done (zerop outstanding))
                  (funcall final-cb))))
             (process-dir-queue
              (lambda (p-tail s-tail)
                (setq emeld--chunk-worker-timer nil)
                (when (and (buffer-live-p buf)
                           (eq (with-current-buffer buf emeld--scan-generation) current-gen))
                  (cond
                   (p-tail
                    (let ((time-limit (+ (float-time) 0.015)))
                      (catch 'emeld--budget-yield
                        (while p-tail
                          (if (> (float-time) time-limit)
                              (throw 'emeld--budget-yield t)
                            (let* ((next-branch (car p-tail))
                                   (sub-entries (nth 0 next-branch))
                                   (sub-prefix (nth 1 next-branch))
                                   (dir-node (nth 2 next-branch)))
                              (cl-incf outstanding)
                              (emeld--entries-to-nodes-progressive
                               sub-entries sub-prefix dir-node buf
                               (lambda (sub-tier-nodes)
                                 (setf (emeld-node-children dir-node) sub-tier-nodes)
                                 (let ((any-diff (cl-some (lambda (n) (not (eq (emeld-node-diff-status n) 'same))) sub-tier-nodes)))
                                   (setf (emeld-node-diff-status dir-node) (if any-diff 'different 'same))))
                               (lambda ()
                                 (let ((now (float-time))
                                       (last-render (with-current-buffer buf emeld--last-render-time)))
                                   (when (or (null last-render) (> (- now last-render) emeld-render-throttle))
                                     (with-current-buffer buf
                                       (setq emeld--last-render-time now)
                                       (emeld--render))))
                                 (cl-decf outstanding)
                                 (funcall finish-check))
                               current-gen))
                            (setq p-tail (cdr p-tail))))))
                    (if p-tail
                        (setq emeld--chunk-worker-timer
                              (run-with-timer 0.001 nil (lambda () (with-current-buffer buf (funcall process-dir-queue p-tail s-tail)))))
                      (with-current-buffer buf (funcall process-dir-queue nil s-tail))))

                   (s-tail
                    (let ((time-limit (+ (float-time) 0.015)))
                      (catch 'emeld--budget-yield
                        (while s-tail
                          (if (> (float-time) time-limit)
                              (throw 'emeld--budget-yield t)
                            (let* ((next-branch (car s-tail))
                                   (sub-entries (nth 0 next-branch))
                                   (sub-prefix (nth 1 next-branch))
                                   (dir-node (nth 2 next-branch)))
                              (let ((is-last (null (cdr s-tail))))
                                (cl-incf outstanding)
                                (emeld--entries-to-nodes-progressive
                                 sub-entries sub-prefix dir-node buf
                                 (lambda (sub-tier-nodes)
                                   (setf (emeld-node-children dir-node) sub-tier-nodes))
                                 (lambda ()
                                   (let ((now (float-time))
                                         (last-render (with-current-buffer buf emeld--last-render-time)))
                                     (when (or is-last (null last-render) (> (- now last-render) emeld-render-throttle))
                                       (with-current-buffer buf
                                         (setq emeld--last-render-time now)
                                         (emeld--render))))
                                   (cl-decf outstanding)
                                   (funcall finish-check))
                                 current-gen)))
                            (setq s-tail (cdr s-tail))))))
                    (if s-tail
                        (setq emeld--chunk-worker-timer
                              (run-with-timer 0.001 nil (lambda () (with-current-buffer buf (funcall process-dir-queue nil s-tail)))))
                      (setq own-queues-done t)
                      (funcall finish-check)))

                   (t
                    (setq own-queues-done t)
                    (funcall finish-check)))))))

          (run-with-timer 0.001 nil (lambda () (with-current-buffer buf (funcall process-dir-queue priority-queue same-queue))))))))))

(defun emeld--read-gitignore-simple-patterns (dir)
  "Read .gitignore in DIR and return simple glob patterns for --exclude.
Strips leading and trailing slashes so patterns like `/.idea' or
`build/' yield basename patterns usable by `diff --exclude'.
Patterns with internal slashes after stripping are skipped."
  (let ((gitignore (expand-file-name ".gitignore" dir))
        (patterns nil))
    (when (file-exists-p gitignore)
      (with-temp-buffer
        (insert-file-contents gitignore)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((raw (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))
                 (line (string-trim raw)))
            (unless (or (string-empty-p line)
                        (string-prefix-p "#" line)
                        (string-prefix-p "!" line))
              (let ((clean line))
                (when (string-prefix-p "/" clean)
                  (setq clean (substring clean 1)))
                (when (string-suffix-p "/" clean)
                  (setq clean (substring clean 0 -1)))
                (unless (string-match-p "/" clean)
                  (push clean patterns)))))
          (forward-line 1))))
    (nreverse patterns)))

(defun emeld--diff-exclude-args ()
  "Return `--exclude=PATTERN' args from active filter groups.
Only includes patterns without `/', since `diff --exclude' matches
the basename component, not the full relative path.
When `emeld-respect-gitignore' is non-nil, also includes simple
patterns from root .gitignore files."
  (let ((args '("--exclude=node_modules")))
    (dolist (group emeld-filter-groups)
      (when (member (car group) emeld--active-filter-groups)
        (dolist (glob (emeld--group-globs group))
          (unless (string-match-p "/" glob)
            (push (concat "--exclude=" glob) args)))))
    (when emeld-respect-gitignore
      (dolist (root (list emeld--left-root emeld--right-root))
        (dolist (pattern (emeld--read-gitignore-simple-patterns root))
          (push (concat "--exclude=" pattern) args))))
    args))

(defun emeld--start-diff (buf)
  "Start async `diff -rq' for content comparison.
Active filter group patterns (without `/') are passed to `diff'
as `--exclude' arguments so diff skips filtered files entirely.
Uses a process filter to stream-parse output incrementally."
  (setq emeld--scan-diff-lines nil)
  (with-current-buffer buf
    (setq emeld--benchmark-start-time (float-time)))
  ;; Compute the gitignore sets asynchronously, in parallel with diff, so
  ;; large repos do not block the UI when the tree is assembled.
  (emeld--prewarm-gitignore emeld--left-root buf)
  (emeld--prewarm-gitignore emeld--right-root buf)
  (let* ((excludes (emeld--diff-exclude-args))
         ;; Always scan differences only.  Identical files (when
         ;; `emeld-show-same' is on) are merged afterwards by
         ;; `emeld--scan-same-async', which is far cheaper than having
         ;; `diff -rqs' report — and `emeld--entries-to-nodes-progressive'
         ;; assemble — every identical file up front.
         (command `("diff" "-rq"
                    ,@excludes
                    ,(expand-file-name emeld--left-root)
                    ,(expand-file-name emeld--right-root)))
         (accum-lines nil)
         (accum-tail nil)
         (partial nil)
         (proc (make-process
                :name "emeld-diff"
                :buffer (generate-new-buffer " *emeld-diff*")
                :command command)))
    (set-process-filter
     proc
     (lambda (proc output)
       (when (buffer-live-p buf)
         (let* ((combined (if partial (concat partial output) output))
                (new-lines (split-string combined "[\n\r]+")))
           (if (string-suffix-p "\n" combined)
               (setq partial nil)
             (setq partial (car (last new-lines))
                   new-lines (butlast new-lines)))
           (when new-lines
             (with-current-buffer buf
               (cl-incf emeld--scan-diff-count (length new-lines))
               ;; Mark the header line dirty so the live count redisplays
               ;; during the scan, not just when point moves.
               (force-mode-line-update))
             (if accum-tail
                 (setcdr accum-tail new-lines)
               (setq accum-lines new-lines))
             (setq accum-tail (last new-lines)))))))
    (set-process-sentinel
     proc
     (lambda (p event)
       (when (emeld--sentinel-done-p event)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let ((p-buf (process-buffer p)))
               (when (and p-buf (buffer-live-p p-buf))
                 (kill-buffer p-buf)))
             (setq emeld--scan-diff-lines (or accum-lines (list)))
              (emeld--build-tree-from-diff buf))))))
    (setq emeld--scan-process proc)))

(defun emeld--parse-diff-line (line left-dir right-dir)
  "Parse a `diff -rq' output LINE into (REL-PATH . STATUS) or nil.
Explicitly handles and strips single quotes added to paths by the shell environment."
  (cl-flet ((clean-quotes (path)
              (if (and (string-prefix-p "'" path)
                       (string-suffix-p "'" path))
                  (substring path 1 (1- (length path)))
                path)))
    (cond
     ((string-match "^Files \\(.+\\) and .+ differ$" line)
      (let ((rel (emeld--strip-dir-prefix (clean-quotes (match-string 1 line)) left-dir)))
        (cons (clean-quotes rel) 'different)))
     ((string-match "^Files \\(.+\\) and .+ are identical$" line)
      (let ((rel (emeld--strip-dir-prefix (clean-quotes (match-string 1 line)) left-dir)))
        (cons (clean-quotes rel) 'same)))
     ((string-match "^Only in \\(.+\\): \\(.+\\)$" line)
      (let* ((dir-part (clean-quotes (match-string 1 line)))
             (name (clean-quotes (match-string 2 line)))
             (full-path (concat (file-name-as-directory dir-part) name))
             (left-prefix (expand-file-name left-dir))
             (rel (if (string-prefix-p left-prefix full-path)
                      (emeld--strip-dir-prefix full-path left-dir)
                    (emeld--strip-dir-prefix full-path right-dir)))
             (side (if (string-prefix-p left-prefix full-path)
                       'left-only 'right-only)))
        (cons (clean-quotes rel) side)))
     (t nil))))

(defun emeld--seed-dir-path-hash (node prefix path-hash)
  "Record every directory descendant of NODE in PATH-HASH by relative path.
PREFIX is NODE's own relative path (\"\" for the root).  Pre-seeding lets
`emeld--get-or-create-dir-node' resolve directories that already exist in
the tree (e.g. from the initial diff scan) with an O(1) lookup instead of
scanning each parent's children."
  (dolist (child (emeld-node-children node))
    (when (emeld-node-is-dir child)
      (let ((rel (if (string-empty-p prefix)
                     (emeld-node-name child)
                   (concat prefix "/" (emeld-node-name child)))))
        (puthash rel child path-hash)
        (emeld--seed-dir-path-hash child rel path-hash)))))

(defun emeld--get-or-create-dir-node (rel-dir-path path-hash root left-root right-root)
  "Return the directory node for REL-DIR-PATH, creating it (and any missing
ancestors) when absent.  PATH-HASH is the authoritative dir index for the
scan: it is pre-seeded with existing tree dirs by `emeld--seed-dir-path-hash'
and updated as new dirs are created, so a miss means the dir is genuinely
new and no per-parent scan is needed."
  (or (gethash rel-dir-path path-hash)
      (let* ((parent-dir (file-name-directory (directory-file-name rel-dir-path)))
             (parent-rel-path (if (or (null parent-dir) (string= parent-dir "./")
                                      (string= parent-dir ""))
                                  ""
                                (directory-file-name parent-dir)))
             (parent-node (if (string-empty-p parent-rel-path)
                              root
                            (emeld--get-or-create-dir-node
                             parent-rel-path path-hash root left-root right-root)))
             (dir-name (file-name-nondirectory (directory-file-name rel-dir-path)))
             (new-dir (emeld-node-create
                       :name dir-name
                       :left-path (concat (file-name-as-directory left-root) rel-dir-path)
                       :right-path (concat (file-name-as-directory right-root) rel-dir-path)
                       :parent parent-node
                       :is-dir t
                       :expanded nil
                       :diff-status 'same)))
        (push new-dir (emeld-node-children parent-node))
        (puthash rel-dir-path new-dir path-hash)
        new-dir)))

(defun emeld--add-same-entry-to-tree (root relpath path-hash left-root right-root)
  (let* ((rel-dir (file-name-directory relpath))
         (parent-rel (if rel-dir (directory-file-name rel-dir) ""))
         (parent-node (if (string-empty-p parent-rel)
                          root
                        (emeld--get-or-create-dir-node
                         parent-rel path-hash root left-root right-root)))
         (file-name (file-name-nondirectory relpath))
         ;; File keys are namespaced so they never collide with the
         ;; string dir-path keys used for directory nodes.
         (file-key (cons 'file relpath)))
    (unless (or (emeld--filtered-p file-name relpath)
                ;; O(1) dedup instead of scanning the parent's children.
                (gethash file-key path-hash))
      (let ((new-file (emeld-node-create
                       :name file-name
                       :left-path (concat (file-name-as-directory left-root) relpath)
                       :right-path (concat (file-name-as-directory right-root) relpath)
                       :parent parent-node
                       :diff-status 'same
                       :is-dir nil)))
        (push new-file (emeld-node-children parent-node))
        (puthash file-key new-file path-hash)))))

(defun emeld--sort-tree-recursive (node)
  (when (emeld-node-children node)
    (setf (emeld-node-children node)
          (sort (emeld-node-children node)
                (lambda (a b)
                  (let ((a-dir (emeld-node-is-dir a))
                        (b-dir (emeld-node-is-dir b)))
                    (if (eq a-dir b-dir)
                        (string< (emeld-node-name a) (emeld-node-name b))
                      a-dir)))))
    (dolist (child (emeld-node-children node))
      (emeld--sort-tree-recursive child))))

(defun emeld--process-same-chunk-worker (buf remaining-lines root left right old-gc path-hash)
  (if (not (buffer-live-p buf))
      (setq gc-cons-threshold old-gc)
    (with-current-buffer buf
      (if (input-pending-p)
          (run-with-timer 0.001 nil #'emeld--process-same-chunk-worker
                          buf remaining-lines root left right old-gc path-hash)
        (let ((chunk-count 0))
          (while (and remaining-lines (< chunk-count emeld-batch-size))
            (let ((line (car remaining-lines)))
              (when (string-match-p "are identical$" line)
                (let ((parsed (emeld--parse-diff-line line left right)))
                  (when (and parsed (eq (cdr parsed) 'same))
                    (emeld--add-same-entry-to-tree root (car parsed) path-hash left right)))))
            (setq remaining-lines (cdr remaining-lines))
            (setq chunk-count (1+ chunk-count)))
          (setq emeld--scan-processed (- emeld--scan-total (length remaining-lines)))
          (force-mode-line-update)
          (if remaining-lines
              (progn
                ;; Throttle intermediate redraws; the final state is always
                ;; rendered once the scan completes (the else branch below).
                (let ((now (float-time)))
                  (when (or (null emeld--last-render-time)
                            (> (- now emeld--last-render-time) emeld-render-throttle))
                    (setq emeld--last-render-time now)
                    (emeld--render)))
                (run-with-timer 0.001 nil #'emeld--process-same-chunk-worker
                                buf remaining-lines root left right old-gc path-hash))
            (emeld--bench-push "same merged")
            (emeld--sort-tree-recursive root)
            (emeld--bench-push "same sorted")
            (clrhash path-hash)
            (setq emeld--status-phase 'done
                  emeld--scan-process nil
                  emeld--scan-buf nil
                  emeld--cached-full-lines t)
            (emeld--render)
            (emeld--update-header-line)
            (emeld--bench-push "same rendered")
            (emeld--bench-report)
            (setq gc-cons-threshold old-gc)))))))

(defun emeld--ancestor-in-set-p (rel set)
  "Non-nil if REL or any of its ancestor directories is a key in SET.
The walk stops at the filesystem-root fixpoint, so it always terminates."
  (let ((path (directory-file-name rel)))
    (catch 'hit
      (while path
        (when (gethash path set) (throw 'hit t))
        (let* ((dir (file-name-directory path))
               (next (and dir (directory-file-name dir))))
          (setq path (and next (not (string= next path)) next))))
      nil)))

(defun emeld--collect-same-exclusions (node prefix skip-files left-only-dirs)
  "Walk NODE (whose relpath is PREFIX) collecting same-merge exclusions.
SKIP-FILES gets every existing file node's relpath; LEFT-ONLY-DIRS gets
each left-only directory's relpath (its whole subtree is then excluded)."
  (dolist (child (emeld-node-children node))
    (let ((rel (if (string-empty-p prefix)
                   (emeld-node-name child)
                 (concat prefix "/" (emeld-node-name child)))))
      (if (emeld-node-is-dir child)
          (if (eq (emeld-node-diff-status child) 'left-only)
              (puthash rel t left-only-dirs)
            (emeld--collect-same-exclusions child rel skip-files left-only-dirs))
        (puthash rel t skip-files)))))

(defun emeld--process-same-files-worker (buf paths root left right old-gc path-hash)
  "Chunked merge of enumerated left-side PATHS (relpaths) as identical files.
A path is added as a `same' node unless it is already known
\(`emeld--same-skip-files': differing/left-only/already-merged) or lies
under a left-only directory (`emeld--same-left-only-dirs').
`emeld--add-same-entry-to-tree' additionally honors the active filters."
  (if (not (buffer-live-p buf))
      (setq gc-cons-threshold old-gc)
    (with-current-buffer buf
      (if (input-pending-p)
          (run-with-timer 0.001 nil #'emeld--process-same-files-worker
                          buf paths root left right old-gc path-hash)
        (let ((chunk-count 0))
          (while (and paths (< chunk-count emeld-batch-size))
            (let ((rel (car paths)))
              (unless (or (gethash rel emeld--same-skip-files)
                          (emeld--ancestor-in-set-p rel emeld--same-left-only-dirs))
                (emeld--add-same-entry-to-tree root rel path-hash left right)))
            (setq paths (cdr paths))
            (setq chunk-count (1+ chunk-count)))
          (setq emeld--scan-processed (- emeld--scan-total (length paths)))
          (force-mode-line-update)
          (if paths
              (progn
                (let ((now (float-time)))
                  (when (or (null emeld--last-render-time)
                            (> (- now emeld--last-render-time) emeld-render-throttle))
                    (setq emeld--last-render-time now)
                    (emeld--render)))
                (run-with-timer 0.001 nil #'emeld--process-same-files-worker
                                buf paths root left right old-gc path-hash))
            (emeld--bench-push "same merged")
            (emeld--sort-tree-recursive root)
            (emeld--bench-push "same sorted")
            (clrhash path-hash)
            (setq emeld--status-phase 'done
                  emeld--scan-process nil
                  emeld--scan-buf nil
                  emeld--cached-full-lines t
                  emeld--same-skip-files nil
                  emeld--same-left-only-dirs nil)
            (emeld--render)
            (emeld--update-header-line)
            (emeld--bench-push "same rendered")
            (emeld--bench-report)
            (setq gc-cons-threshold old-gc)))))))

(defun emeld--scan-same-list (buf)
  "Find identical files by listing the left tree and subtracting known paths.
See `emeld-same-files-method'.  Runs `emeld-list-command' asynchronously on
the left root, then merges the leftover (identical) files into the tree."
  (with-current-buffer buf
    (let ((skip (make-hash-table :test 'equal))
          (lod (make-hash-table :test 'equal)))
      (emeld--collect-same-exclusions emeld--root-node "" skip lod)
      (setq emeld--same-skip-files skip
            emeld--same-left-only-dirs lod))
    (let* ((left emeld--left-root)
           (right emeld--right-root)
           (root emeld--root-node)
           (accum nil) (accum-tail nil) (partial nil)
           (default-directory (file-name-as-directory (expand-file-name left)))
           (proc (make-process
                  :name "emeld-list-same"
                  :buffer (generate-new-buffer " *emeld-list-same*")
                  :command emeld-list-command
                  :connection-type 'pipe
                  :noquery t)))
      (set-process-filter
       proc
       (lambda (_p output)
         (when (buffer-live-p buf)
           (let* ((combined (if partial (concat partial output) output))
                  (lines (split-string combined "\n")))
             (if (string-suffix-p "\n" combined)
                 (setq partial nil)
               (setq partial (car (last lines))
                     lines (butlast lines)))
             (let ((rels (delq nil
                               (mapcar (lambda (l)
                                         (cond ((string-empty-p l) nil)
                                               ((string-prefix-p "./" l) (substring l 2))
                                               (t l)))
                                       lines))))
               (when rels
                 (with-current-buffer buf
                   (cl-incf emeld--scan-diff-count (length rels))
                   (force-mode-line-update))
                 (if accum-tail (setcdr accum-tail rels) (setq accum rels))
                 (setq accum-tail (last rels))))))))
      (set-process-sentinel
       proc
       (lambda (p event)
         (when (emeld--sentinel-done-p event)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (let ((pbuf (process-buffer p)))
                 (when (buffer-live-p pbuf) (kill-buffer pbuf)))
               (emeld--bench-push "same list done")
               (let ((old-gc gc-cons-threshold)
                     (path-hash (make-hash-table :test 'equal)))
                 (emeld--seed-dir-path-hash root "" path-hash)
                 (emeld--bench-push "same seeded")
                 (setq gc-cons-threshold (* 128 1024 1024))
                 (setq emeld--status-phase 'building-tree
                       emeld--scan-total (length accum)
                       emeld--scan-processed 0
                       emeld--last-render-time nil)
                 (emeld--update-header-line)
                 (run-with-timer 0.001 nil #'emeld--process-same-files-worker
                                 buf accum root left right old-gc path-hash)))))))
      (setq emeld--scan-process proc))))

(defun emeld--scan-same-async (buf)
  (with-current-buffer buf
    (emeld--stop-scan)
    (setq emeld--status-phase 'running-diff
          emeld--cached-full-lines nil
          emeld--scan-diff-count 0
          emeld--scan-processed 0
          emeld--scan-total 0)
    ;; Start a fresh benchmark run for the identical-files stack, independent
    ;; of the initial scan's report (which already reset these).
    (when emeld-benchmark
      (setq emeld--benchmark-data nil
            emeld--benchmark-start-time (float-time)))
    ;; Idempotent: a no-op if the initial scan already cached these roots.
    (emeld--prewarm-gitignore emeld--left-root buf)
    (emeld--prewarm-gitignore emeld--right-root buf)
    (emeld--update-header-line)
    (if (eq emeld-same-files-method 'list)
        (emeld--scan-same-list buf)
      (emeld--scan-same-diff buf))))

(defun emeld--scan-same-diff (buf)
  "Find identical files by re-running `diff -rqs' (see `emeld-same-files-method')."
  (with-current-buffer buf
    (let* ((excludes (emeld--diff-exclude-args))
           (left emeld--left-root)
           (right emeld--right-root)
           (root emeld--root-node)
           (command `("diff" "-rqs" ,@excludes
                      ,(expand-file-name left)
                      ,(expand-file-name right)))
           (proc (make-process
                  :name "emeld-diff-same"
                  :buffer (generate-new-buffer " *emeld-diff-same*")
                  :command command)))
      (process-put proc 'emeld-buf buf)
      (process-put proc 'emeld-left left)
      (process-put proc 'emeld-right right)
      (process-put proc 'emeld-root root)
      ;; Count entries as they stream in so the header shows live progress
      ;; (`[Scanning… N entries]') during this phase, while still letting the
      ;; output accumulate in the process buffer for the sentinel to read.
      (set-process-filter
       proc
       (lambda (p output)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (cl-incf emeld--scan-diff-count (cl-count ?\n output))
             (force-mode-line-update)))
         (let ((pbuf (process-buffer p)))
           (when (buffer-live-p pbuf)
             (with-current-buffer pbuf
               (goto-char (point-max))
               (insert output))))))
      (set-process-sentinel
       proc
       (lambda (p event)
         (when (emeld--sentinel-done-p event)
           (let ((target-buf (process-get p 'emeld-buf)))
             (when (buffer-live-p target-buf)
               (with-current-buffer target-buf
                 (let* ((p-buf (process-buffer p))
                        (raw (if (buffer-live-p p-buf)
                                 (with-current-buffer p-buf (buffer-string))
                               ""))
                        (lines (split-string raw "[\n\r]+" t))
                        (old-gc gc-cons-threshold)
                        (ctx-left (process-get p 'emeld-left))
                        (ctx-right (process-get p 'emeld-right))
                         (ctx-root (process-get p 'emeld-root))
                         (path-hash (make-hash-table :test 'equal)))
                    (when (buffer-live-p p-buf)
                      (kill-buffer p-buf))
                    (emeld--bench-push "same diff done")
                    ;; Seed the dir index with the tree built by the initial
                    ;; scan so identical files merge into existing dirs in O(1).
                    (emeld--seed-dir-path-hash ctx-root "" path-hash)
                    (emeld--bench-push "same seeded")
                    (setq gc-cons-threshold (* 128 1024 1024))
                    (setq emeld--status-phase 'building-tree
                          emeld--scan-total (length lines)
                          emeld--scan-processed 0
                          emeld--last-render-time nil)
                    (emeld--update-header-line)
                    (run-with-timer 0.001 nil #'emeld--process-same-chunk-worker
                                    target-buf lines ctx-root ctx-left ctx-right old-gc path-hash))))))))
      (setq emeld--scan-process proc))))

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

(defun emeld--git-ignored-key (root-dir)
  "Return the canonical `emeld--gitignore-cache' key for ROOT-DIR, or nil."
  (and root-dir (file-name-as-directory (expand-file-name root-dir))))

(defun emeld--parse-git-ignored-output (output)
  "Parse NUL-separated `git ls-files' OUTPUT into a hash set of paths.
`-z' output is unquoted, so paths with spaces or newlines survive intact.
Fully-ignored directories arrive with a trailing slash, stored here
without it; files inside them are matched via the ancestor walk in
`emeld--git-ignored-p'."
  (let ((set (make-hash-table :test #'equal)))
    (dolist (entry (split-string output "\0" t))
      (puthash (directory-file-name entry) t set))
    set))

(defun emeld--git-work-tree-p (dir)
  "Return non-nil if DIR exists and is inside a git work tree.
This runs a cheap (sub-millisecond) `git rev-parse'; the expensive
`ls-files' enumeration is left to the async path."
  (and (file-directory-p dir)
       (executable-find "git")
       (let ((default-directory (file-name-as-directory dir)))
         (eq 0 (call-process "git" nil nil nil
                             "rev-parse" "--is-inside-work-tree")))))

(defun emeld--compute-git-ignored-set (dir)
  "Synchronously compute the git-ignored set for DIR, or the symbol `none'.
This blocks on `git ls-files' and is only used as a fallback for direct
callers (e.g. tests); the scan pre-warms the cache asynchronously via
`emeld--prewarm-gitignore' so this is not hit on the hot path."
  (if (not (emeld--git-work-tree-p dir))
      'none
    (let ((default-directory (file-name-as-directory dir)))
      (with-temp-buffer
        (if (eq 0 (call-process "git" nil t nil "ls-files" "-z"
                                "-o" "-i" "--exclude-standard" "--directory"))
            (emeld--parse-git-ignored-output (buffer-string))
          'none)))))

(defun emeld--prewarm-gitignore (root-dir buf)
  "Start an async `git ls-files' for ROOT-DIR, caching the result in BUF.
Runs in parallel with the `diff' scan so the ignored set is ready by the
time the tree is assembled, keeping the UI responsive on large repos.
A no-op when `emeld-respect-gitignore' is nil or ROOT-DIR is already
cached/pending for this scan."
  (when (and emeld-respect-gitignore root-dir)
    (with-current-buffer buf
      (unless emeld--gitignore-cache
        (setq emeld--gitignore-cache (make-hash-table :test #'equal)))
      (let ((key (emeld--git-ignored-key root-dir)))
        (when (and key (null (gethash key emeld--gitignore-cache)))
          (if (not (emeld--git-work-tree-p key))
              (puthash key 'none emeld--gitignore-cache)
            ;; Mark pending so lookups don't fall back to a blocking compute
            ;; while the async process is in flight.
            (puthash key 'pending emeld--gitignore-cache)
            (let* ((default-directory key)
                   (pbuf (generate-new-buffer " *emeld-gitignore*"))
                   (proc (make-process
                          :name "emeld-gitignore"
                          :buffer pbuf
                          :command '("git" "ls-files" "-z" "-o" "-i"
                                     "--exclude-standard" "--directory")
                          :connection-type 'pipe
                          :noquery t)))
              (push proc emeld--gitignore-procs)
              (set-process-sentinel
               proc
               (lambda (p event)
                 (when (emeld--sentinel-done-p event)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (setq emeld--gitignore-procs (delq p emeld--gitignore-procs))
                       (puthash key
                                (if (and (buffer-live-p pbuf)
                                         (eq 0 (process-exit-status p)))
                                    (emeld--parse-git-ignored-output
                                     (with-current-buffer pbuf (buffer-string)))
                                  'none)
                                emeld--gitignore-cache)))
                   (when (buffer-live-p pbuf) (kill-buffer pbuf))))))))))))

(defun emeld--git-ignored-set (root-dir)
  "Return the git-ignored path set for ROOT-DIR, or nil.
Reads the per-scan cache populated asynchronously by
`emeld--prewarm-gitignore'.  Returns nil while the async query is still
`pending (best effort: a file is simply not hidden until the set lands),
when ROOT-DIR is not a git work tree, or when ROOT-DIR is missing.  Falls
back to a synchronous compute only for direct callers outside a scan."
  (when root-dir
    (unless emeld--gitignore-cache
      (setq emeld--gitignore-cache (make-hash-table :test #'equal)))
    (let* ((key (emeld--git-ignored-key root-dir))
           (cached (gethash key emeld--gitignore-cache 'unset)))
      (cond
       ((hash-table-p cached) cached)
       ((memq cached '(none pending)) nil)
       (t                               ; unset: not pre-warmed, compute now
        (let ((set (emeld--compute-git-ignored-set key)))
          (puthash key set emeld--gitignore-cache)
          (and (hash-table-p set) set)))))))

(defun emeld--git-ignored-p (rel-path root-dir)
  "Check if REL-PATH under ROOT-DIR is ignored by Git.
Honors `emeld-respect-gitignore'.  Looks REL-PATH up in the cached
`git ls-files' result for ROOT-DIR (see `emeld--git-ignored-set'), and
matches files nested inside an ignored directory by walking ancestor
paths.  The walk stops as soon as it can no longer ascend, so an absolute
or root path can never spin forever."
  (when (and emeld-respect-gitignore rel-path (not (string-empty-p rel-path)))
    (let ((set (emeld--git-ignored-set root-dir)))
      (when set
        (let ((path (directory-file-name rel-path)))
          (catch 'ignored
            (while path
              (when (gethash path set)
                (throw 'ignored t))
              (let* ((dir (file-name-directory path))
                     (next (and dir (directory-file-name dir))))
                ;; Stop at the fixpoint (e.g. "/") or when there is no
                ;; parent separator left, otherwise the walk never ends.
                (setq path (and next (not (string= next path)) next))))
            nil))))))

(defun emeld--filtered-p (name rel-path)
  "Check if NAME or REL-PATH is filtered.
Check active filter groups and, when `emeld-respect-gitignore' is non-nil,
query git for .gitignore rules on both sides."
  (or (cl-some (lambda (group)
                 (when (member (car group) emeld--active-filter-groups)
                   (cl-some (lambda (glob)
                              (let ((rx (emeld--glob-to-regexp glob)))
                                (if (string-match-p "/" glob)
                                    (string-match-p rx rel-path)
                                  (string-match-p rx name))))
                            (emeld--group-globs group))))
               emeld-filter-groups)
      (emeld--git-ignored-p rel-path emeld--left-root)
      (emeld--git-ignored-p rel-path emeld--right-root)))

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
                "Filter groups are toggled from the transient menu (~ca~, ~cb~, ...) or with ~M-x emeld-toggle-filter-group~.\n\n")
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
      (setq emeld--marked-paths (make-hash-table :test 'equal))
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

;;;; ─── Presets (saved directory pairs) ─────────────────────────────

(defun emeld--read-preset (prompt)
  "Read a saved preset name via `completing-read' with PROMPT.
Candidates are annotated with their left/right directories."
  (unless emeld-presets
    (user-error "No saved presets (use `emeld-save-preset')"))
  (let ((completion-extra-properties
         (list :annotation-function
               (lambda (name)
                 (let ((e (assoc name emeld-presets)))
                   (when e
                     (concat "   "
                             (abbreviate-file-name (nth 1 e)) " ↔ "
                             (abbreviate-file-name (nth 2 e)))))))))
    (completing-read prompt (mapcar #'car emeld-presets) nil t)))

(defun emeld-save-preset (name)
  "Save the current comparison's directories as preset NAME.
Recall it later with `emeld-load-preset'.  Reusing an existing name
overwrites it.  Must be called from an emeld comparison buffer."
  (interactive
   (progn
     (unless (and emeld--left-root emeld--right-root)
       (user-error "No active emeld comparison to save"))
     (list (completing-read
            "Save preset as: " (mapcar #'car emeld-presets) nil nil
            ;; Pre-fill an editable suggested name (RET to accept as-is).
            (format "%s <-> %s"
                    (file-name-nondirectory (directory-file-name emeld--left-root))
                    (file-name-nondirectory (directory-file-name emeld--right-root)))))))
  (unless (and emeld--left-root emeld--right-root)
    (user-error "No active emeld comparison to save"))
  (when (string-empty-p (string-trim name))
    (user-error "Preset name must not be empty"))
  (setq emeld-presets
        (cons (list name emeld--left-root emeld--right-root)
              (assoc-delete-all name emeld-presets)))
  (message "Saved preset \"%s\": %s ↔ %s" name
           (abbreviate-file-name emeld--left-root)
           (abbreviate-file-name emeld--right-root)))

;;;###autoload
(defun emeld-load-preset (name)
  "Load saved preset NAME and start the comparison.
NAME is chosen from `emeld-presets' via completion."
  (interactive (list (emeld--read-preset "Load preset: ")))
  (let ((entry (assoc name emeld-presets)))
    (unless entry
      (user-error "No preset named \"%s\"" name))
    (emeld-diff (nth 1 entry) (nth 2 entry))))

(defun emeld-delete-preset (name)
  "Delete saved preset NAME from `emeld-presets'."
  (interactive (list (emeld--read-preset "Delete preset: ")))
  (setq emeld-presets (assoc-delete-all name emeld-presets))
  (message "Deleted preset \"%s\"" name))

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
    (define-key map (kbd "TAB") #'emeld-cycle-subtree)
    (define-key map (kbd "<backtab>") #'emeld-cycle-visibility)
    (define-key map (kbd "S-TAB")     #'emeld-cycle-visibility)
    (define-key map (kbd "[")   #'emeld-collapse-all)
    (define-key map (kbd "]")   #'emeld-expand-all)
    (define-key map (kbd "\\")  #'emeld-expand-changes)
    (define-key map (kbd "RET") #'emeld-action)
    (define-key map (kbd "e")   #'emeld-action)
    (define-key map (kbd "=")   #'emeld-action)
    (define-key map (kbd "f")   #'emeld-find-file)
    (define-key map (kbd "v")   #'emeld-find-file)
    (define-key map (kbd "g")   #'emeld-refresh)
    (define-key map (kbd "r")   #'emeld-redraw)
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
    (define-key map (kbd "m")   #'emeld-mark)
    (define-key map (kbd "u")   #'emeld-unmark)
    (define-key map (kbd "U")   #'emeld-unmark-all)
    (define-key map (kbd "* m") #'emeld-mark-modified)
    (define-key map (kbd "* l") #'emeld-mark-left-only)
    (define-key map (kbd "* c") #'emeld-mark-to-copy)
    (define-key map (kbd "S")   #'emeld-toggle-show-same)
    (define-key map (kbd "M")   #'emeld-toggle-show-different)
    (define-key map (kbd "N")   #'emeld-toggle-show-added)
    (define-key map (kbd "T")   #'emeld-toggle-tree-lines)
    (define-key map (kbd "s")   #'emeld-toggle-show-strikethrough)
    (define-key map (kbd "G")   #'emeld-toggle-gitignore)
    (define-key map (kbd ">")   #'emeld-increase-indent)
    (define-key map (kbd "<")   #'emeld-decrease-indent)
    (define-key map (kbd "}")   #'emeld-grow-column)
    (define-key map (kbd "{")   #'emeld-shrink-column)
    map)
  "Keymap for `emeld-mode'.")

(define-derived-mode emeld-mode special-mode "Emeld"
  "Major mode for Meld-Inspired Local Diff."
  (buffer-disable-undo)
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t)
  (make-local-variable 'emeld-show-same)
  (make-local-variable 'emeld-show-different)
  (make-local-variable 'emeld-show-added)
  (make-local-variable 'emeld-show-strikethrough)
  (make-local-variable 'emeld-respect-gitignore)
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
    
    ;; Skip mark char, prefixes, icons, and spaces on current side
    (let ((start-on-side (point)))
      (while (and (< (point) (line-end-position))
                  (< (point) limit)
                  (looking-at "[]* └├│─\\[+-]"))
        (forward-char 1))

      ;; If we reached the limit on the left side, back up slightly to stay on the left
      (when (and (not on-right) (>= (current-column) width))
        (goto-char start-on-side)
        (skip-chars-forward "* └├│─")))
    
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
                          (looking-at "[]* └├│─\\[+-]"))
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
        (emeld--scan-same-async (current-buffer))
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

(defun emeld-toggle-gitignore ()
  "Toggle whether `.gitignore' rules are respected, then rescan.
Unlike the view toggles this changes which files are scanned, so it
re-runs the comparison rather than just re-rendering."
  (interactive)
  (setq emeld-respect-gitignore (not emeld-respect-gitignore))
  (message "Respect .gitignore: %s" (if emeld-respect-gitignore "on" "off"))
  (emeld-refresh))

(defun emeld-toggle-expand ()
  "Toggle expansion of the node at point."
  (interactive)
  (let ((node (get-text-property (point) 'emeld-node)))
    (when (and node (emeld-node-is-dir node))
      (emeld--maybe-populate-lazy-dir node)
      (setf (emeld-node-expanded node) (not (emeld-node-expanded node)))
      (let ((pos (point)))
        (emeld--render)
        (goto-char pos)))))

(defun emeld--walk-set-expanded (node value)
  "Recursively set the :expanded slot on NODE and its descendant dirs to VALUE."
  (when (and node (emeld-node-is-dir node))
    (setf (emeld-node-expanded node) value)
    (dolist (child (emeld-node-children node))
      (emeld--walk-set-expanded child value))))

(defun emeld--walk-set-dirs-only (node)
  "Expand intermediate dir nodes, collapse leaf dir nodes (those with no sub-dirs).
Shows the directory skeleton without revealing the file leaves inside."
  (when (and node (emeld-node-is-dir node))
    (let ((has-subdir (cl-some #'emeld-node-is-dir (emeld-node-children node))))
      (setf (emeld-node-expanded node) has-subdir)
      (dolist (child (emeld-node-children node))
        (emeld--walk-set-dirs-only child)))))

(defun emeld--subtree-fully-expanded-p (node)
  "Return non-nil when every dir descendant of NODE is expanded."
  (catch 'unexpanded
    (dolist (child (emeld-node-children node))
      (when (emeld-node-is-dir child)
        (unless (emeld-node-expanded child)
          (throw 'unexpanded nil))
        (unless (emeld--subtree-fully-expanded-p child)
          (throw 'unexpanded nil))))
    t))

(defun emeld-collapse-all ()
  "Collapse every directory in the tree.  The root stays expanded."
  (interactive)
  (when emeld--root-node
    (dolist (child (emeld-node-children emeld--root-node))
      (emeld--walk-set-expanded child nil))
    (emeld--render)))

(defun emeld-expand-all ()
  "Expand every directory in the tree."
  (interactive)
  (when emeld--root-node
    (dolist (child (emeld-node-children emeld--root-node))
      (emeld--walk-set-expanded child t))
    (emeld--render)))

(defun emeld-expand-dirs-only ()
  "Expand intermediate directories; collapse leaf directories.
Reveals the directory skeleton without showing the file leaves inside.
A directory is considered a leaf when none of its children are dirs."
  (interactive)
  (when emeld--root-node
    (dolist (child (emeld-node-children emeld--root-node))
      (emeld--walk-set-dirs-only child))
    (emeld--render)))

(defun emeld-expand-changes ()
  "Expand directories that lead to a change, collapse those that don't.
A directory is expanded when its `diff-status' is not `same' — i.e. it
contains a modified, added, or removed descendant.  Useful for focusing
the view after \\[emeld-collapse-all], or after toggling same files on."
  (interactive)
  (when emeld--root-node
    (cl-labels ((walk (node)
                  (when (emeld-node-is-dir node)
                    (setf (emeld-node-expanded node)
                          (not (eq (emeld-node-diff-status node) 'same)))
                    (dolist (child (emeld-node-children node))
                      (walk child)))))
      (dolist (child (emeld-node-children emeld--root-node))
        (walk child))
      (emeld--render))))

(defun emeld-cycle-subtree ()
  "Cycle the subtree at point through FOLDED → DIRS → SUBTREE, org-mode style.

If point is on a directory:
- FOLDED: just this directory collapsed.
- DIRS: this directory expanded; intermediate sub-dirs expanded; leaf
  sub-dirs collapsed so file leaves are hidden.
- SUBTREE: this directory and every descendant dir expanded; files visible.

Repeated invocations cycle FOLDED → DIRS → SUBTREE → FOLDED.  The current
state is inferred from the actual expansion levels — no per-node state
is stored, so external commands (TAB on another dir, `[`, `]`, …) compose
naturally."
  (interactive)
  (let ((node (get-text-property (point) 'emeld-node)))
    (cond
     ((not (and node (emeld-node-is-dir node)))
      (user-error "Not on a directory"))
     ((not (emeld-node-expanded node))
      ;; FOLDED → DIRS
      (emeld--maybe-populate-lazy-dir node)
      (setf (emeld-node-expanded node) t)
      (dolist (child (emeld-node-children node))
        (emeld--walk-set-dirs-only child))
      (message "subtree: dirs"))
     ((not (emeld--subtree-fully-expanded-p node))
      ;; DIRS → SUBTREE
      (emeld--walk-set-expanded node t)
      (message "subtree: all"))
     (t
      ;; SUBTREE → FOLDED
      (setf (emeld-node-expanded node) nil)
      (message "subtree: folded")))
    (let ((pos (point)))
      (emeld--render)
      (goto-char pos))))

(defvar-local emeld--cycle-state nil
  "Most recent state from `emeld-cycle-visibility': `top', `dirs', `changes', or nil.")

(defun emeld-cycle-visibility ()
  "Cycle the tree's expansion state in three steps, like org-mode's `S-TAB'.

Order:
- TOP: only the root's immediate child directories visible (rest collapsed).
- DIRS: every intermediate directory expanded, leaf directories collapsed
  so the directory skeleton is shown without the file leaves inside.
- CHANGES: every directory that leads to a change expanded; identical
  subtrees collapsed.  File leaves are visible under leaf dirs.

Repeated invocations cycle TOP → DIRS → CHANGES → TOP."
  (interactive)
  (let ((next (pcase emeld--cycle-state
                ('top 'dirs)
                ('dirs 'changes)
                (_ 'top))))
    (setq emeld--cycle-state next)
    (pcase next
      ('top     (emeld-collapse-all)    (message "Visibility: TOP (top-level dirs)"))
      ('dirs    (emeld-expand-dirs-only) (message "Visibility: DIRS (directory skeleton)"))
      ('changes (emeld-expand-changes)   (message "Visibility: CHANGES (modified files)")))))

(defun emeld--stop-scan ()
  "Cancel the active async scan and clean up processes."
  (when (and emeld--scan-process
             (process-live-p emeld--scan-process))
    (delete-process emeld--scan-process))
  (dolist (p emeld--gitignore-procs)
    (when (process-live-p p) (delete-process p)))
  (setq emeld--gitignore-procs nil
        emeld--scan-process nil
        emeld--scan-diff-lines nil
        emeld--scan-buf nil))

(defun emeld-redraw ()
  "Re-render the current tree without rescanning.
Recomputes the column width from the current window width and redraws,
preserving point and expansion state.  Use this after resizing the
window or frame; for a fresh comparison use `emeld-refresh' (\\[emeld-refresh])."
  (interactive)
  (unless emeld--root-node
    (user-error "No comparison loaded"))
  (emeld--render))

(defun emeld-refresh ()
  "Refresh the comparison asynchronously.
Uses `make-process' to run `diff -rq' as an OS-level async process,
so Emacs stays fully responsive during the scan."
  (interactive)
  (emeld--stop-scan)
  (setq emeld--status-phase 'running-diff)
  (let* ((buf (current-buffer))
         (root emeld--root-node)
         (node (get-text-property (point) 'emeld-node)))
    (setq emeld--scan-buf buf
          emeld--benchmark-data nil
          emeld--saved-expanded (emeld--collect-expanded root)
          emeld--saved-point-path (when node
                                    (or (emeld-node-left-path node)
                                        (emeld-node-right-path node)))
          emeld--saved-point-col (current-column)
          emeld--cached-full-lines nil
          ;; Reset progress counters so the header line starts from zero.
          emeld--scan-diff-count 0
          emeld--scan-processed 0
          emeld--scan-total 0
          ;; Drop memoized gitignore sets so .gitignore edits are honored.
          emeld--gitignore-cache nil)
    (setf (emeld-node-children root) nil)
    (emeld--update-header-line)
    (emeld--render)
    (emeld--start-diff buf)))

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

(defun emeld--maybe-populate-lazy-dir (node)
  "Populate NODE's children if it is a lazy (unexpanded) single-sided directory."
  (when (and (emeld-node-is-dir node)
             (null (emeld-node-children node))
             (memq (emeld-node-diff-status node) '(left-only right-only)))
    (let* ((status (emeld-node-diff-status node))
           (side-path (if (eq status 'left-only)
                          (emeld-node-left-path node)
                        (emeld-node-right-path node)))
           ;; Strip side-path itself, not the comparison root: entries must be
           ;; relative to NODE, otherwise the full path-from-root gets nested
           ;; under NODE producing a duplicated subtree.
           (base-dir (file-name-as-directory (expand-file-name side-path)))
           (node-rel (emeld--node-relpath node))
           (sub-files (directory-files-recursively side-path "" t))
           (entries nil))
      (dolist (f sub-files)
        (let* ((abs-f (expand-file-name f))
               (rel (if (string-prefix-p base-dir abs-f)
                        (substring abs-f (length base-dir))
                      abs-f)))
          (when (string-prefix-p "/" rel)
            (setq rel (substring rel 1)))
          (unless (string-empty-p rel)
            (push (cons rel status) entries))))
      (setf (emeld-node-children node)
            (emeld--entries-to-nodes entries node-rel node)))))

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

(defun emeld-mark ()
  "Mark the node at point (Dired-style)."
  (interactive)
  (let ((node (get-text-property (point) 'emeld-node)))
    (unless node (user-error "No node at point"))
    (let ((rel (emeld--node-relpath node)))
      (puthash rel t emeld--marked-paths)
      (emeld--render)
      (forward-line 1)
      (emeld--move-to-node-name (>= (current-column) emeld--column-width)))))

(defun emeld-unmark ()
  "Unmark the node at point."
  (interactive)
  (let ((node (get-text-property (point) 'emeld-node)))
    (unless node (user-error "No node at point"))
    (let ((rel (emeld--node-relpath node)))
      (remhash rel emeld--marked-paths)
      (emeld--render)
      (forward-line 1)
      (emeld--move-to-node-name (>= (current-column) emeld--column-width)))))

(defun emeld-unmark-all ()
  "Unmark all nodes."
  (interactive)
  (clrhash emeld--marked-paths)
  (emeld--render)
  (message "Marks cleared"))

(defun emeld--mark-nodes-by-status (statuses)
  "Mark every file node whose `diff-status' is in STATUSES.
Adds to any existing marks and returns the number newly marked."
  (let ((count 0))
    (cl-labels ((walk (nodes)
                  (dolist (node nodes)
                    (if (emeld-node-is-dir node)
                        (walk (emeld-node-children node))
                      (when (memq (emeld-node-diff-status node) statuses)
                        (puthash (emeld--node-relpath node) t emeld--marked-paths)
                        (cl-incf count))))))
      (when emeld--root-node
        (walk (emeld-node-children emeld--root-node))))
    count))

(defun emeld--mark-and-report (statuses what)
  "Mark file nodes whose status is in STATUSES; report as WHAT.  Re-renders."
  (unless emeld--root-node
    (user-error "No comparison loaded"))
  (let ((n (emeld--mark-nodes-by-status statuses)))
    (emeld--render)
    (message "Marked %d %s%s" n what (if (= n 1) "" "s"))))

(defun emeld-mark-modified ()
  "Mark all modified (differing) files, Dired-style.
Adds to any existing marks; the marked set can then be acted on, e.g.
copied with `emeld-copy'."
  (interactive)
  (emeld--mark-and-report '(different) "modified file"))

(defun emeld-mark-left-only ()
  "Mark all files that exist only on the left (i.e. created on the left)."
  (interactive)
  (emeld--mark-and-report '(left-only) "left-only file"))

(defun emeld-mark-to-copy ()
  "Mark everything to copy left→right for syncing: modified + left-only files.
With these marked, `emeld-copy' (\\[emeld-copy]) copies each to the right,
making the right directory match the left for those files."
  (interactive)
  (emeld--mark-and-report '(different left-only) "file to copy"))

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

(defun emeld--copy-one (source dest-root)
  "Copy a single file or directory at SOURCE to DEST-ROOT preserving path.
Creates missing parent directories."
  (let* ((rel-path (file-relative-name source
                                       (if (string-prefix-p (expand-file-name emeld--left-root)
                                                            (expand-file-name source))
                                           emeld--left-root
                                         emeld--right-root)))
         (dest (directory-file-name (expand-file-name rel-path dest-root)))
         (dest-parent (file-name-directory dest)))
    (unless (file-directory-p dest-parent)
      (when (file-exists-p dest-parent) (delete-file dest-parent))
      (make-directory dest-parent t))
    (if (file-directory-p source)
        (if (fboundp 'copy-directory-contents)
            (copy-directory source dest t t t)
          (when (and (file-exists-p dest) (not (file-directory-p dest)))
            (delete-file dest))
          (copy-directory source dest t t))
      (copy-file source dest t))
    (message "Copied %s" rel-path)))

(defun emeld-copy ()
  "Copy marked files/dirs to the other side.
When marks exist, all marked nodes are copied.
Otherwise, copy the node at point.
Creates missing parent directories on the destination side if needed."
  (interactive)
  (let ((marked (let (paths)
                  (maphash (lambda (k _) (push k paths)) emeld--marked-paths)
                  (nreverse paths))))
    (if marked
        (let ((count 0) (errors 0))
          (when (yes-or-no-p (format "Copy %d marked node(s) to %s? "
                                     (length marked) emeld--right-root))
            (dolist (rel marked)
              (let ((left (expand-file-name rel emeld--left-root))
                    (right (expand-file-name rel emeld--right-root)))
                (cond ((file-exists-p left)
                       (condition-case nil
                           (progn (emeld--copy-one left emeld--right-root)
                                  (setq count (1+ count)))
                         (error (setq errors (1+ errors)))))
                      ((file-exists-p right)
                       (condition-case nil
                           (progn (emeld--copy-one right emeld--left-root)
                                  (setq count (1+ count)))
                         (error (setq errors (1+ errors))))))))
            (emeld-refresh)
            (clrhash emeld--marked-paths)
            (message "Copied %d node(s)%s" count
                     (if (> errors 0) (format " (%d errors)" errors) ""))))
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
                    (unless (file-directory-p dest-parent)
                      (when (file-exists-p dest-parent) (delete-file dest-parent))
                      (make-directory dest-parent t))
                    (if (file-directory-p source)
                        (if (fboundp 'copy-directory-contents)
                            (copy-directory source dest t t t)
                          (when (and (file-exists-p dest) (not (file-directory-p dest)))
                            (delete-file dest))
                          (copy-directory source dest t t))
                      (copy-file source dest t))
                    (emeld-refresh)
                    (message "Copied %s" (emeld-node-name node)))
                (error (message "Copy failed: %s" (error-message-string err)))))))))))

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
  "Return a string for toggle LABEL showing ENABLED state.
Enabled toggles are bold; disabled toggles use the plain header-line
face.  No colour is applied, only weight, so the status display stays on
the standard foreground and background."
  (if enabled (propertize label 'face 'bold) label))

(defun emeld--display-state-string ()
  "Return a compact string showing current display toggle states."
  (let ((groups (mapcar (lambda (g) (concat "[" g "]"))
                        emeld--active-filter-groups)))
    (concat
     (emeld--toggle-indicator "S" emeld-show-same)
     (emeld--toggle-indicator "M" emeld-show-different)
     (emeld--toggle-indicator "N" emeld-show-added)
     (emeld--toggle-indicator "T" emeld-show-tree-lines)
     (emeld--toggle-indicator "s" emeld-show-strikethrough)
     (when groups
       (concat " " (mapconcat #'identity groups " "))))))

(defun emeld--progress-bar (fraction &optional width)
  "Return a WIDTH-character text bar visualizing FRACTION (0.0-1.0).
WIDTH defaults to 10.  Uses block characters so the fill is obvious at a
glance in the header line."
  (let* ((width (or width 10))
         (fraction (max 0.0 (min 1.0 (or fraction 0.0))))
         (filled (round (* fraction width))))
    (concat "[" (make-string filled ?█)
            (make-string (- width filled) ?░) "]")))

(defun emeld--scan-progress-string ()
  "Return the live scan-progress segment for the header line, or nil.
Shows a growing entry tally while `diff' runs and a determinate progress
bar while the tree is parsed; falls back to an indeterminate label during
node assembly (when the parse total has been consumed).  Rendered with no
face so it uses the standard header-line foreground and background."
  (pcase emeld--status-phase
    ('running-diff
     (if (> emeld--scan-diff-count 0)
         (format " [Scanning… %d entries]" emeld--scan-diff-count)
       " [Scanning…]"))
    ('building-tree
     (cond
      ((zerop emeld--scan-total) " [Building tree…]")
      ((< emeld--scan-processed emeld--scan-total)
       (let ((frac (/ (float emeld--scan-processed) emeld--scan-total)))
         (format " [Building tree %s %d%%]"
                 (emeld--progress-bar frac) (round (* 100 frac)))))
      (t " [Assembling nodes…]")))
    ('done " [Ready]")
    (_     " [Idle]")))

(defun emeld--update-header-line ()
  "Install the header-line format showing scan progress, status and legend.
The format is an `:eval' form, so it re-evaluates on each redisplay and
reflects the live scan counters without needing to be re-installed."
  (setq header-line-format
        '(:eval
          (concat (emeld--scan-progress-string)
                  " [" (emeld--display-state-string) "] "))))

(defun emeld--current-node ()
  "Return the emeld-node text-property at point."
  (get-text-property (point) 'emeld-node))

(defun emeld--render ()
  "Render the current visible tree into the Emacs comparison buffer, preserving point."
  (let* ((inhibit-read-only t)
         (inhibit-modification-hooks t)
         (inhibit-point-motion-hooks t)
         (current-node (emeld--current-node))
         (current-col (current-column))
         (target-path (and current-node (emeld-node-relative-path current-node)))
         (target-is-dir (and current-node (emeld-node-is-dir current-node)))
         (fallback-line (line-number-at-pos)))
    
    (setq emeld--column-width (/ (* (window-width) emeld-column-ratio) 100))
    (buffer-disable-undo)
    (erase-buffer)
    (remove-overlays)
    
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
    
    ;; Restore point based on path matching or structural layout line
    (or (and target-path
             (emeld--move-to-node-by-path target-path target-is-dir current-col))
        (progn
          (goto-char (point-min))
          (forward-line (1- (min fallback-line (count-lines (point-min) (point-max)))))))
    
    (set-buffer-modified-p nil)))

(defun emeld-node-relative-path (node)
  "Construct the canonical relative path string for NODE from roots."
  (let ((components nil)
        (curr node))
    (while (and curr (emeld-node-parent curr))
      (push (emeld-node-name curr) components)
      (setq curr (emeld-node-parent curr)))
    (string-join components "/")))

(defun emeld--move-to-node-by-path (rel-path is-dir target-col)
  "Search the buffer to position point on the node matching REL-PATH.
Returns t if successfully positioned, otherwise nil."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found) (not (eobp)))
        (let ((node (emeld--current-node)))
          (when (and node
                     (string= (emeld-node-relative-path node) rel-path)
                     (eq (emeld-node-is-dir node) is-dir))
            (setq found (point))))
        (forward-line 1)))
    (if found
        (progn
          (goto-char found)
          (move-to-column target-col)
          t)
      nil)))

(defun emeld--node-visible-p (node)
  "Determine if NODE should be visible based on status filters.
The root node is always visible.  Directory nodes are always shown
so collapsed folder rows remain navigable."
  (or (eq node emeld--root-node)
      (and (emeld-node-is-dir node)
           (not (eq (emeld-node-diff-status node) 'same)))
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
          (let ((start (point))
                (mark-char (if (and emeld--marked-paths
                                    (gethash (emeld--node-relpath node) emeld--marked-paths))
                               (propertize "*" 'face 'font-lock-keyword-face)
                             " ")))
            (insert mark-char " ")
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
    ("S" (lambda () (if emeld-show-same "[x] S" "[ ] S"))
     (lambda () (interactive) (emeld-toggle-show-same)) :transient t)
    ("N" (lambda () (if emeld-show-added "[x] N" "[ ] N"))
     (lambda () (interactive) (emeld-toggle-show-added)) :transient t)
    ("M" (lambda () (if emeld-show-different "[x] M" "[ ] M"))
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
    ("s" (lambda () (if emeld-show-strikethrough "[x] strikethrough" "[ ] strikethrough"))
     (lambda () (interactive) (emeld-toggle-show-strikethrough)) :transient t)
    ("G" (lambda () (if emeld-respect-gitignore "[x] gitignore" "[ ] gitignore"))
     (lambda () (interactive) (emeld-toggle-gitignore)))
    ("[" "collapse all"  emeld-collapse-all :transient t)
    ("]" "expand all"    emeld-expand-all :transient t)
    ("\\" "expand to changes" emeld-expand-changes :transient t)
    ("<backtab>" "cycle visibility" emeld-cycle-visibility :transient t)
    (">" "indent more"  emeld-increase-indent :transient t)
    ("<" "indent less"  emeld-decrease-indent :transient t)
    ("}" "pane wider"   emeld-grow-column :transient t)
    ("{" "pane narrow"  emeld-shrink-column :transient t)]
   ["Navigate"
    ("g"   "rescan"             emeld-refresh)
    ("r"   "redraw (resize)"    emeld-redraw)
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
   ["Mark"
    ("m"   "mark"            emeld-mark :transient t)
    ("u"   "unmark"          emeld-unmark :transient t)
    ("* m" "modified"        emeld-mark-modified :transient t)
    ("* l" "left-only (new)" emeld-mark-left-only :transient t)
    ("* c" "→ copy right"    emeld-mark-to-copy :transient t)
    ("U"   "unmark all"      emeld-unmark-all :transient t)]
   ["Preset"
    ("ps" "save current" emeld-save-preset)
    ("pl" "load…"        emeld-load-preset)
    ("pd" "delete…"      emeld-delete-preset)]
   ])

(define-key emeld-mode-map (kbd "?") #'emeld-dispatch)

(provide 'emeld)
;;; emeld.el ends here
