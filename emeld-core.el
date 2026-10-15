;;; emeld-core.el --- Meld-Inspired Local Diff: shared core -*- lexical-binding: t; -*-

;; Keywords: files, tools
;; Version: 0.48.0

;;; Commentary:
;; A simple side-by-side directory comparison tool inspired by Meld.
;; `emeld-diff' compares two directories; `emeld-git-diff' compares two
;; git revisions (or a revision against the working tree / index),
;; showing the same hierarchical tree of added, deleted and modified files.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'subr-x)
(require 'transient)
(require 'diff-mode)  ; diff-* faces for the inline diff preview

(declare-function find-library-name "find-func" (library))
(declare-function dired-dwim-target-directory "dired-aux")
(defvar savehist-additional-variables)

;; git-diff mode lives in emeld-git.el (loaded by the umbrella `emeld').  Core
;; dispatches into it from `emeld-refresh', the file ops, the render and the
;; mode keymap/transient; declare those entry points so core compiles alone.
(declare-function emeld--start-git-diff "emeld-git" (buf))
(declare-function emeld--build-git-tree "emeld-git" (buf entries))
(declare-function emeld--insert-git-commit-block "emeld-git" ())
(declare-function emeld--git-action "emeld-git" (node on-left))
(declare-function emeld--git-soft-diff "emeld-git" (node))
(declare-function emeld--git-blob-buffer "emeld-git" (rev relpath))
(declare-function emeld--git-diff-show "emeld-git" (relpath what))
(declare-function emeld--read-git-rev "emeld-git" (prompt repo &optional default))
(declare-function emeld-git-toggle-commit-message "emeld-git" ())
(declare-function emeld-git-history-older "emeld-git" ())
(declare-function emeld-git-history-newer "emeld-git" ())
(declare-function emeld-git-diff "emeld-git" (repo left-rev right-rev))
(declare-function emeld-git-step-history "emeld-git" (repo start))

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
  :type '(repeat
          (choice :tag "Filter group"
                  (cons :tag "Name, active flag, globs"
                        (string :tag "Group name")
                        (cons (boolean :tag "Active by default")
                              (repeat :tag "Glob patterns" string)))
                  (cons :tag "Name and globs (inactive by default)"
                        (string :tag "Group name")
                        (repeat :tag "Glob patterns" string))))
  :group 'emeld)

(put 'emeld-filter-groups 'safe-local-variable
     (lambda (val)
       (and (listp val)
            (cl-every (lambda (entry)
                        (and (listp entry) (stringp (car entry))
                             ;; Optional t/nil "active by default" flag,
                             ;; mirroring `emeld--group-globs'.
                             (let ((rest (cdr entry)))
                               (when (memq (car rest) '(t nil))
                                 (setq rest (cdr rest)))
                               (cl-every #'stringp rest))))
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

(defcustom emeld-dir-prefix ""
  "Prefix string shown before directory names to distinguish from files.
Defaults to no prefix for maximum compatibility (terminals and Windows
builds may not render emoji or icons).  Set to e.g. \"📁 \" for an icon."
  :type 'string
  :group 'emeld)

(defcustom emeld-fold-collapsed-indicator "[+] "
  "Indicator shown before a collapsed (folded) directory name.
Rendered just before `emeld-dir-prefix'.  Plain ASCII by default for
maximum terminal/Windows compatibility; set it together with
`emeld-fold-expanded-indicator' to icons or triangles to mimic Meld, e.g.
\"▸ \" collapsed / \"▾ \" expanded.  Keep the two indicators the same
display width so folded and unfolded directory names stay aligned."
  :type 'string
  :group 'emeld)

(defcustom emeld-fold-expanded-indicator "[-] "
  "Indicator shown before an expanded (unfolded) directory name.
See `emeld-fold-collapsed-indicator' for details and alignment notes."
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

(defcustom emeld-tree-density 'wide
  "How tightly the tree is indented.  One of three symbols:

`wide' (the default) indents each level one space wider than the branch
connector, so a child's connector hangs one column past the parent's
content (`│  └─file') and reads as hooking into the parent's subtree.

`compact' matches the continuation indent to the branch width for a
tighter tree (`│ └─file') that sits well in the available space.

`extreme' collapses each level to a single column regardless of
`emeld-indent-offset' — the connector is a lone glyph with no dashes
(`│└file') — for the densest possible tree.  Especially handy in the
narrow `emeld-sidebar'.

Whatever the density, files and directories share the same indent, so
siblings always align under their parent.  Cycle compact → extreme →
wide → compact with `emeld-cycle-tree-density' (\\`z')."
  :type '(choice (const :tag "Wide (default)" wide)
                 (const :tag "Compact" compact)
                 (const :tag "Extreme (1 column/level)" extreme))
  :group 'emeld)

(defcustom emeld-show-strikethrough t
  "Whether to show strikethrough for orphan files on the missing side.
When non-nil, a file that exists only on one side shows its tree path
and filename with strikethrough on the other side, mimicking Meld's
display.  When nil, the other side shows nothing for the orphan."
  :type 'boolean
  :group 'emeld)

(defcustom emeld-show-files t
  "Whether to show regular files (non-directory entries).
When nil, only the directory hierarchy is displayed."
  :type 'boolean
  :group 'emeld)

(defcustom emeld-column-ratio 50
  "Width of the left column as a percentage of the window width (10-90).
Defaults to 50 for equal-width panes."
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

(defcustom emeld-ignore-whitespace nil
  "How `diff'/`git diff' should treat whitespace when comparing file content.
nil       compare exactly (default);
`change'  ignore changes in the amount of whitespace (`diff -b');
`all'     ignore all whitespace (`diff -w').
Applies to the difference scan, the identical-file merge, the quick and
inline diffs, and (in git mode) `git diff' — both tools accept these
flags.  Because it changes which files count as differing, toggling it
re-runs the comparison rather than just re-rendering."
  :type '(choice (const :tag "Compare exactly" nil)
                 (const :tag "Ignore whitespace amount (-b)" change)
                 (const :tag "Ignore all whitespace (-w)" all))
  :group 'emeld)

(defun emeld--whitespace-args ()
  "Return the whitespace diff flags for `emeld-ignore-whitespace'.
The returned list is valid for both `diff' and `git diff'."
  (pcase emeld-ignore-whitespace
    ('change '("-b"))
    ('all '("-w"))
    (_ nil)))

(defcustom emeld-copy-method 'auto
  "How `emeld-copy' transfers files and directories between the two sides.
- `auto'    : use rsync when `emeld-rsync-command' is found on `exec-path',
              otherwise fall back to Emacs's built-in copy.
- `rsync'   : always use rsync; error up front if it is not available.
- `builtin' : always use `copy-file' / `copy-directory'.
rsync only sends the parts of a file that changed, preserves metadata,
and recurses efficiently, so it is faster and more robust for large or
already-partly-synced trees.  See `emeld-rsync-options'."
  :type '(choice (const :tag "Use rsync if available" auto)
                 (const :tag "Always rsync" rsync)
                 (const :tag "Always built-in copy" builtin))
  :group 'emeld)

(defcustom emeld-rsync-command "rsync"
  "Name or absolute path of the rsync executable.
Used by `emeld-copy' when `emeld-copy-method' permits rsync."
  :type 'string
  :group 'emeld)

(defcustom emeld-rsync-options '("-a")
  "Options passed to rsync for each copy, as a list of strings.
`-a' (archive) preserves permissions, timestamps, symlinks and recurses.
Add e.g. \"-z\" for compression.  `--delete' is controlled separately by
`emeld-copy-mirror', not by this list."
  :type '(repeat string)
  :group 'emeld)

(defcustom emeld-copy-mirror nil
  "When non-nil, rsync directory copies are exact mirrors (`--delete').
The destination directory is made to match the source exactly, removing
files that exist only on the destination — so \"copy left→right\" becomes
a true mirror rather than a merge.  Only affects rsync copies of
directories (`emeld-copy-method' must allow rsync); single files and the
built-in copy are unaffected, since `--delete' has nothing to prune for a
single file and `copy-directory' cannot delete.  To mirror a whole tree,
copy a directory node (or a left-only directory) with this enabled.
Because it deletes files, `emeld-copy' labels the action as a mirror and
warns before proceeding."
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

(defun emeld--ensure-dir-locals (root)
  "Create `.dir-locals.el' at ROOT holding the default filter groups.
Only acts when no such file exists directly at ROOT; an existing
file is never touched.  The written value is the current
`emeld-filter-groups', so a fresh comparison always has a populated
file for `emeld-edit-dir-locals' to jump to.  Failures (e.g. a
read-only directory) only warn; the comparison proceeds with the
in-memory defaults."
  (let ((file (expand-file-name dir-locals-file root)))
    (unless (file-exists-p file)
      (condition-case err
          (progn
            (with-temp-file file
              (insert ";;; Directory Local Variables\n"
                      ";;; See Info node `(emacs) Directory Variables' for more information.\n\n")
              (pp `((nil . ((emeld-filter-groups . ,emeld-filter-groups))))
                  (current-buffer)))
            (message "Created %s with default filters"
                     (abbreviate-file-name file)))
        (error (message "Could not create %s: %s"
                        (abbreviate-file-name file)
                        (error-message-string err)))))))

(cl-defstruct (emeld-node
               (:constructor emeld-node-create))
  name
  (name-right nil)    ; display name for the right side (defaults to `name')
  left-path
  right-path
  parent
  children
  (expanded t)
  (diff-status nil) ; 'same, 'different, 'left-only, 'right-only, 'unscanned
  (is-dir nil)
  (scanned t)       ; nil for a directory left unscanned by a depth-limited scan
  (is-filtered nil)
  (inline-diff nil)) ; cached propertized diff lines when expanded inline (TAB on a file)

(defvar-local emeld--scan-max-depth nil
  "Directory-depth limit for the initial scan, or nil for a full scan.
Set from the prefix argument to `emeld-diff'; honored by `emeld-refresh'.
When non-nil, directories below this depth are left unscanned and marked,
to be drilled into with `\\[universal-argument] g' (`emeld-refresh-subdir').")
(defvar-local emeld--scan-process nil
  "Currently running `make-process' for the async diff scan.")
(defvar-local emeld--copy-proc nil
  "In-flight async rsync process for `emeld-copy', or nil when idle.")
(defvar-local emeld--scan-diff-lines nil
  "Diff status lines from `diff -rq' output.")
(defvar-local emeld--scan-buf nil
  "Buffer being scanned (for sentinel callbacks).")
(defvar-local emeld--chunk-worker-timer nil
  "Holds the active chunking timer to prevent overlapping timer execution.")
(defvar-local emeld--scan-generation 0
  "Incremented on each new scan start.
Workers check this to abort stale operations.")
(defvar-local emeld--active-filter-groups nil
  "List of currently active filter group names.")
(defvar-local emeld--left-root nil)
(defvar-local emeld--right-root nil)
(defvar-local emeld--root-node nil)
(defvar-local emeld--cached-full-lines nil
  "Non-nil once an identical-files (show-same) merge has populated the tree.
Lets `emeld-toggle-show-same' avoid re-scanning when toggling the view.")
(defvar-local emeld--git-mode nil
  "Non-nil when this buffer compares git revisions rather than directories.
Set by `emeld-git-diff'.  Switches the scan to `git diff --name-status'
and reroutes file operations to git blobs instead of on-disk files.")
(defvar-local emeld--git-repo nil
  "Absolute path (with trailing slash) of the git work-tree root in git mode.")
(defvar-local emeld--git-left-rev nil
  "Left (old) revision in git mode.
A revision string (commit/branch/tag), or the symbol `working' (the
working tree) or `staged' (the index).")
(defvar-local emeld--git-right-rev nil
  "Right (new) revision in git mode.  See `emeld--git-left-rev' for the shape.")
(defvar-local emeld--git-left-commit nil
  "Commit metadata plist for `emeld--git-left-rev', or nil.
See `emeld--git-commit-info' for the plist shape; nil when the rev is the
working tree, the index, or could not be resolved to a commit.")
(defvar-local emeld--git-right-commit nil
  "Commit metadata plist for `emeld--git-right-rev', or nil.")
(defvar-local emeld--git-commit-expanded nil
  "When non-nil, the git commit-message block shows the full body.
When nil (the default) only each commit's subject line is shown.  Toggle
with `emeld-git-toggle-commit-message' (\\`c').")
(defvar-local emeld--git-history nil
  "First-parent commit SHAs (newest first) for history stepping, or nil.
Set by `emeld-git-step-history'; `emeld--git-history-index' points at the
current commit, which is compared against its parent.")
(defvar-local emeld--git-history-index 0
  "Index into `emeld--git-history' of the commit currently being viewed.
0 is the newest (the starting revision); larger is older.")
(defvar-local emeld--git-expand-changes-pending nil
  "One-shot flag: expand changed dirs once the next git tree finishes.
Set by `emeld--git-history-apply' when `emeld-git-history-auto-expand' is
non-nil; consumed in the `emeld--build-git-tree' completion callback,
overriding the restored expansion state for that scan only.")
(defvar-local emeld--git-head-sha nil
  "Resolved SHA of the repo's HEAD, cached for labelling.
Refreshed on each git scan so `emeld--git-rev-label' can show a revision
that points at HEAD as \"HEAD\" rather than a bare abbreviated SHA.")
(defvar-local emeld--column-width nil)
(defvar-local emeld--saved-expanded nil
  "Saved expanded directory paths to restore after refresh.")
(defvar-local emeld--saved-dirs nil
  "All directory paths that existed when `emeld--saved-expanded' was captured.
Lets `emeld--restore-expanded' tell a directory the user had collapsed from
one that has only just appeared (e.g. a subtree revealed by toggling
`emeld-respect-gitignore' off), so new content is shown rather than folded.")
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
Values can be `idle', `running-diff', `building-tree', or `done'.")

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
  "Hash table of marked nodes (Dired-style marking).
Keys are relative paths; each value is a list of marked sides — a subset
of (left right).  A node can be marked on either side independently.")
(defvar-local emeld--hidden-paths nil
  "Relative paths temporarily hidden this session by `emeld-hide-at-point'.
The renderer skips these nodes (and their subtrees).  Buffer-local and
deliberately *not* reset on rescan, so a hide survives `emeld-refresh'
within the session; it is never persisted to `.dir-locals.el' (use
`emeld-add-filter-at-point' for that).  Cleared by `emeld-unhide-all'.")
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
    (cond
     ;; Git mode: `git diff --name-status' reports only files (never bare
     ;; directories), and the content lives in history, not on disk — so do
     ;; not probe the filesystem.  Carry the relpath as the side "path" for
     ;; whichever revision(s) hold the file; file operations resolve it via
     ;; `git show'.
     (emeld--git-mode
      (emeld-node-create
       :name name
       :left-path (and (memq status '(same different left-only)) relpath)
       :right-path (and (memq status '(same different right-only)) relpath)
       :parent parent
       :diff-status status))
     ((memq status '(left-only right-only))
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
             :diff-status status))))
     (t
      (emeld-node-create
       :name name
       :left-path (and (memq status '(same different left-only))
                       emeld--left-root
                       (concat (file-name-as-directory emeld--left-root) relpath))
       :right-path (and (memq status '(same different right-only))
                        emeld--right-root
                        (concat (file-name-as-directory emeld--right-root) relpath))
       :parent parent
       :diff-status status)))))

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
                                 :left-path (if emeld--git-mode sub-prefix
                                              (and emeld--left-root
                                                   (concat (file-name-as-directory emeld--left-root) sub-prefix)))
                                 :right-path (if emeld--git-mode sub-prefix
                                               (and emeld--right-root
                                                    (concat (file-name-as-directory emeld--right-root) sub-prefix)))
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
responsive while a large comparison streams in.  Intermediate redraws are
additionally skipped whenever there is pending input, so an in-progress
scan never blocks scrolling or typing.  The final result is always
rendered once the scan completes regardless of this value.

Only consulted when `emeld-scan-live-render' is non-nil; otherwise no
intermediate redraws happen at all."
  :type 'number
  :group 'emeld)

(defcustom emeld-scan-live-render nil
  "Whether to redraw the tree progressively while a scan builds it.

Each redraw erases and re-inserts the whole visible tree, so it grows more
expensive as the tree fills and, once started, runs to completion before
Emacs can service input — that is the chief cause of jerkiness while a
large comparison streams in.  When nil (the default) emeld keeps Emacs
maximally responsive during a scan: it updates only the lightweight
header-line progress indicator as work proceeds and draws the finished
tree once, when the scan completes.  Set to t to watch the tree grow
live (throttled by `emeld-render-throttle'), at some cost to
responsiveness on large repositories.

Applies to both the initial `diff -rq' build and the identical-file merge
enabled by `emeld-show-same'.  The completed tree is always rendered when
the scan finishes regardless of this setting."
  :type 'boolean
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
             (scan-gen emeld--scan-generation)
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
               (if (or (not (buffer-live-p buf))
                       ;; A newer scan (or `emeld-stop-scan') bumped the
                       ;; generation: abort this stale build.
                       (/= scan-gen (buffer-local-value 'emeld--scan-generation buf)))
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
                          (emeld--entries-to-nodes-progressive
                          entries "" root buf
                          (lambda (top-level-children)
                            (setf (emeld-node-children root) top-level-children)
                            (emeld--render))
                          (lambda ()
                             (with-current-buffer buf
                               (setf (emeld-node-diff-status root)
                                     (if (cl-some (lambda (c) (not (eq (emeld-node-diff-status c) 'same))
                                                               )
                                                  (emeld-node-children root))
                                         'different 'same))
                               (setq emeld--scan-process nil
                                     emeld--scan-buf nil)
                               (emeld--restore-expanded root)
                               (unless emeld-show-same
                                 (setq emeld--status-phase 'done))
                               (emeld--render)
                               (emeld--bench-push "tree built")
                               (emeld--bench-report)
                               (setq gc-cons-threshold old-gc)
                               (if emeld-show-same
                                   ;; Differences are placed; merge identical
                                   ;; files via the fast path rather than having
                                   ;; assembled them all from diff -rqs.
                                   (emeld--scan-same-async buf)
                                 (emeld--update-header-line))))
                          scan-gen))))))))
          (run-with-timer 0.001 nil #'process-lines-chunk))))))

(defun emeld--entries-to-nodes-progressive (entries prefix parent buf initial-render-cb final-cb &optional gen)
  "Convert ENTRIES into nodes progressively.
Prioritizes branches containing modifications or differences so they render
first, allowing identical/same branches to populate quietly afterward."
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

    ;; 3. Check if this is an implicit matching node branch that needs physical
    ;; unrolling.  Skipped in git mode: there is no on-disk tree to enumerate
    ;; for a historical revision, and `git diff' already yields full paths.
    (when (and (null files) (hash-table-empty-p dirs) (not (string-empty-p prefix))
               (not emeld--git-mode))
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
    
    ;; Index the file nodes built above by name for O(1) dedup, so the second
    ;; pass (which re-walks `files', possibly grown by the backfill) does not do
    ;; an O(n) `cl-find-if' per entry — that was O(n^2) for a wide directory.
    (let ((existing-names (make-hash-table :test 'equal))
          (tail files))
      (dolist (n file-nodes) (puthash (emeld-node-name n) t existing-names))
      (while tail
        (let ((entry (car tail)))
          (let* ((path (car entry))
                 (status (cdr entry)))
            (unless (or (string-empty-p path) (gethash path dir-names)
                        (gethash path existing-names))
              (let ((sub-rel (if (string-empty-p prefix) path (concat prefix "/" path))))
                (push (emeld--make-file-node sub-rel parent status) file-nodes)
                (puthash path t existing-names)))))
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
                               ;; In git mode the side "path" is just the
                               ;; relpath (both sides, so the dir renders on
                               ;; both); otherwise it is the on-disk path.
                               :left-path (and (not (string-empty-p sub-prefix))
                                               (if emeld--git-mode sub-prefix
                                                 (and emeld--left-root
                                                      (concat (file-name-as-directory emeld--left-root) sub-prefix))))
                               :right-path (and (not (string-empty-p sub-prefix))
                                                (if emeld--git-mode sub-prefix
                                                  (and emeld--right-root
                                                       (concat (file-name-as-directory emeld--right-root) sub-prefix))))
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
                                 (when emeld-scan-live-render
                                   (let ((now (float-time))
                                         (last-render (with-current-buffer buf emeld--last-render-time)))
                                     (when (and (not (input-pending-p))
                                                (or (null last-render)
                                                    (> (- now last-render) emeld-render-throttle)))
                                       (with-current-buffer buf
                                         (setq emeld--last-render-time now)
                                         (emeld--render)))))
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
                                   (when emeld-scan-live-render
                                     (let ((now (float-time))
                                           (last-render (with-current-buffer buf emeld--last-render-time)))
                                       (when (and (not (input-pending-p))
                                                  (or is-last (null last-render)
                                                      (> (- now last-render) emeld-render-throttle)))
                                         (with-current-buffer buf
                                           (setq emeld--last-render-time now)
                                           (emeld--render)))))
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
                    ,@(emeld--whitespace-args)
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
     (lambda (_proc output)
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
Explicitly handles and strips single quotes added to paths by the shell.
The boundary between the two paths in a \"Files A and B differ\" line is
anchored on the known RIGHT-DIR prefix (the literal directory passed to
`diff') rather than a greedy \" and \", so a filename that itself contains
\" and \" no longer splits the path at the wrong place."
  (cl-flet ((clean-quotes (path)
              (if (and (string-prefix-p "'" path)
                       (string-suffix-p "'" path))
                  (substring path 1 (1- (length path)))
                path)))
    ;; The right path always begins with RIGHT-DIR (optionally single-quoted),
    ;; so use that as the split anchor instead of the first " and ".
    (let ((sep (concat " and '?"
                       (regexp-quote (file-name-as-directory
                                      (expand-file-name right-dir))))))
      (cond
       ((string-match (concat "^Files \\(.+\\)" sep ".* differ$") line)
        (let ((rel (emeld--strip-dir-prefix (clean-quotes (match-string 1 line)) left-dir)))
          (cons (clean-quotes rel) 'different)))
       ((string-match (concat "^Files \\(.+\\)" sep ".* are identical$") line)
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
       (t nil)))))

(defun emeld--prefix-to-depth (arg)
  "Convert a raw prefix ARG to a scan depth (>=1), or nil for a full scan."
  (cond ((null arg) nil)
        ((integerp arg) (max 1 arg))
        (t 1)))                         ; bare C-u, `-', etc.

(defun emeld--diff-level-entries (left-sub right-sub)
  "Compare LEFT-SUB and RIGHT-SUB one level deep (non-recursive `diff -q').
Return a list of (NAME . KIND); KIND is `different', `left-only',
`right-only' or `common-dir'.  Honors the active filter groups and
`.gitignore' via the same `--exclude' args as a full scan.  Subdirectories
present on both sides are reported (as `common-dir') but not descended."
  (let ((args (append '("-q")
                      (emeld--whitespace-args)
                      (emeld--diff-exclude-args)
                      (list (expand-file-name left-sub)
                            (expand-file-name right-sub))))
        (left-base (directory-file-name (expand-file-name left-sub)))
        (entries nil))
    (with-temp-buffer
      (apply #'call-process "diff" nil t nil args)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-match "^Files \\(.+\\) and .+ differ$" line)
            (push (cons (file-name-nondirectory (match-string 1 line)) 'different)
                  entries))
           ((string-match "^Only in \\(.+\\): \\(.+\\)$" line)
            (let ((side (if (string-prefix-p left-base
                                            (expand-file-name (match-string 1 line)))
                            'left-only 'right-only)))
              (push (cons (match-string 2 line) side) entries)))
           ((string-match "^Common subdirectories: \\(.+\\) and .+$" line)
            (push (cons (file-name-nondirectory (match-string 1 line)) 'common-dir)
                  entries))))
        (forward-line 1)))
    (nreverse entries)))

(defun emeld--scan-one-level (node depth max-depth)
  "Scan NODE one directory level and create its children in place.
DEPTH is NODE's depth (0 = root).  Common subdirectories are descended
while DEPTH+1 < MAX-DEPTH; otherwise (and for one-sided directories) the
child directory is created but left unscanned.  Return the (CHILD . NEXT-DEPTH)
pairs that should be scanned next."
  (let* ((rel (emeld--node-relpath node))
         (lp (file-name-as-directory (emeld-node-left-path node)))
         (rp (file-name-as-directory (emeld-node-right-path node)))
         (entries (emeld--diff-level-entries lp rp))
         (children nil)
         (to-descend nil))
    (dolist (e entries)
      (let* ((name (car e)) (kind (cdr e))
             (child-rel (if (string-empty-p rel) name (concat rel "/" name))))
        (unless (emeld--filtered-p name child-rel)
          (pcase kind
            ('different
             (push (emeld-node-create
                    :name name :parent node :diff-status 'different
                    :left-path (concat lp name) :right-path (concat rp name))
                   children))
            ((or 'left-only 'right-only)
             (let* ((path (concat (if (eq kind 'left-only) lp rp) name))
                    (is-dir (file-directory-p path)))
               (push (emeld-node-create
                      :name name :parent node :diff-status kind :is-dir is-dir
                      :expanded nil
                      :left-path (and (eq kind 'left-only) path)
                      :right-path (and (eq kind 'right-only) path)
                      ;; A one-sided directory is shown but its contents are
                      ;; not enumerated here; mark it unscanned so it is drilled
                      ;; with `C-u G'.
                      :scanned (not is-dir))
                     children)))
            ('common-dir
             (let ((cnode (emeld-node-create
                           :name name :parent node :is-dir t :expanded nil
                           :diff-status 'same
                           :left-path (concat lp name) :right-path (concat rp name))))
               (if (< (1+ depth) max-depth)
                   (push (cons cnode (1+ depth)) to-descend)
                 (setf (emeld-node-scanned cnode) nil
                       (emeld-node-diff-status cnode) 'unscanned))
               (push cnode children)))))))
    (setf (emeld-node-children node)
          (sort children
                (lambda (a b)
                  (let ((ad (emeld-node-is-dir a)) (bd (emeld-node-is-dir b)))
                    (if (eq ad bd)
                        (string< (emeld-node-name a) (emeld-node-name b))
                      ad)))))
    (nreverse to-descend)))

(defun emeld--finalize-dir-statuses (node)
  "Bottom-up: set each directory's status and `scanned' flag from descendants.
A directory is `different' if any descendant is a change, else `unscanned'
if any descendant (or itself) was left unscanned, else `same'.  One-sided
directory statuses (`left-only'/`right-only') are preserved."
  (when (emeld-node-is-dir node)
    (let ((any-change nil)
          (any-unscanned (not (emeld-node-scanned node))))
      (dolist (c (emeld-node-children node))
        (if (emeld-node-is-dir c)
            (progn
              (emeld--finalize-dir-statuses c)
              (when (memq (emeld-node-diff-status c) '(different left-only right-only))
                (setq any-change t))
              (unless (emeld-node-scanned c) (setq any-unscanned t)))
          (when (memq (emeld-node-diff-status c) '(different left-only right-only))
            (setq any-change t))))
      (unless (memq (emeld-node-diff-status node) '(left-only right-only))
        (setf (emeld-node-diff-status node)
              (cond (any-change 'different)
                    (any-unscanned 'unscanned)
                    (t 'same))))
      (when any-unscanned (setf (emeld-node-scanned node) nil)))))

(defun emeld--start-bounded-diff (buf depth)
  "Build the tree for BUF by scanning only DEPTH directory levels.
Uses non-recursive `diff -q' level by level (driven by an idle timer so
Emacs stays responsive), leaving deeper and one-sided directories
unscanned for drilling with `\\[universal-argument] g'.  Aborts if the
buffer's `emeld--scan-generation' changes (a newer scan started)."
  (with-current-buffer buf
    (let ((gen emeld--scan-generation)
          (root emeld--root-node)
          (queue (list (cons emeld--root-node 0)))
          step)
      (setq emeld--status-phase 'building-tree)
      (emeld--update-header-line)
      (setq step
            (lambda ()
              (when (and (buffer-live-p buf)
                         (= gen (buffer-local-value 'emeld--scan-generation buf)))
                (with-current-buffer buf
                  (cond
                   ((input-pending-p) (run-with-idle-timer 0.05 nil step))
                   ((null queue)
                    (emeld--finalize-dir-statuses root)
                    (setq emeld--status-phase 'done)
                    (emeld--update-header-line)
                    (emeld--render))
                   (t
                    (let* ((item (car queue))
                           (more (emeld--scan-one-level
                                  (car item) (cdr item) depth)))
                      (setq queue (append (cdr queue) more))
                      (emeld--render)
                      (run-with-timer 0 nil step))))))))
      (emeld--render)
      (funcall step))))

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
          ;; Track progress incrementally; `(length remaining-lines)' each chunk
          ;; would be O(n) per chunk, i.e. O(n^2) over a large repo.
          (cl-incf emeld--scan-processed chunk-count)
          (force-mode-line-update)
          (if remaining-lines
              (progn
                ;; Intermediate redraws are off by default: rebuilding the
                ;; whole tree each batch blocks input until it finishes, so we
                ;; let the header-line progress bar carry feedback and draw the
                ;; finished tree once on completion (the else branch below).
                ;; `emeld-scan-live-render' opts back into live growth, still
                ;; throttled and skipped while input is pending.
                (when emeld-scan-live-render
                  (let ((now (float-time)))
                    (when (and (not (input-pending-p))
                               (or (null emeld--last-render-time)
                                   (> (- now emeld--last-render-time) emeld-render-throttle)))
                      (setq emeld--last-render-time now)
                      (emeld--render))))
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
          ;; Track progress incrementally; `(length paths)' each chunk would be
          ;; O(n) per chunk, i.e. O(n^2) over a large repo.
          (cl-incf emeld--scan-processed chunk-count)
          (force-mode-line-update)
          (if paths
              (progn
                ;; Intermediate redraws are off by default (see
                ;; `emeld-scan-live-render'): a full re-render each batch blocks
                ;; scrolling/typing until it finishes.  The header-line progress
                ;; keeps updating regardless, and the final tree is always
                ;; rendered on completion below.
                (when emeld-scan-live-render
                  (let ((now (float-time)))
                    (when (and (not (input-pending-p))
                               (or (null emeld--last-render-time)
                                   (> (- now emeld--last-render-time) emeld-render-throttle)))
                      (setq emeld--last-render-time now)
                      (emeld--render))))
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

(defun emeld--git-ls-command (rev)
  "Return the argv listing every tracked file at REV in the current repo.
REV is a revision string, or the symbol `working' or `staged'."
  (let ((repo (list "git" "-C" (directory-file-name emeld--git-repo))))
    (cond
     ((memq rev '(working staged)) (append repo (list "ls-files" "-z")))
     ((stringp rev) (append repo (list "ls-tree" "-r" "--name-only" "-z" rev)))
     (t (error "Cannot list files for revision %S" rev)))))

(defun emeld--scan-same-git (buf)
  "Merge files identical between the two revisions into the tree in BUF.
The left revision's full file list is enumerated (`git ls-tree' or
`git ls-files'); any path the initial `git diff' did not report as changed
is identical in the right revision, so it is added as a `same' node via the
shared `emeld--process-same-files-worker'."
  (with-current-buffer buf
    (let ((skip (make-hash-table :test 'equal))
          (lod (make-hash-table :test 'equal)))
      (emeld--collect-same-exclusions emeld--root-node "" skip lod)
      (setq emeld--same-skip-files skip
            emeld--same-left-only-dirs lod))
    (let* ((left emeld--left-root)
           (right emeld--right-root)
           (root emeld--root-node)
           (command (emeld--git-ls-command emeld--git-left-rev))
           (chunks nil)
           (proc (make-process
                  :name "emeld-git-ls"
                  :buffer (generate-new-buffer " *emeld-git-ls*")
                  :connection-type 'pipe
                  :noquery t
                  :command command)))
      (set-process-filter proc (lambda (_p output) (push output chunks)))
      (set-process-sentinel
       proc
       (lambda (p event)
         (when (emeld--sentinel-done-p event)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (let ((pbuf (process-buffer p)))
                 (when (buffer-live-p pbuf) (kill-buffer pbuf)))
               (emeld--bench-push "same list done")
               (let* ((raw (apply #'concat (nreverse chunks)))
                      (paths (split-string raw "\0" t))
                      (old-gc gc-cons-threshold)
                      (path-hash (make-hash-table :test 'equal)))
                 (emeld--seed-dir-path-hash root "" path-hash)
                 (emeld--bench-push "same seeded")
                 (setq gc-cons-threshold (* 128 1024 1024))
                 (setq emeld--status-phase 'building-tree
                       emeld--scan-total (length paths)
                       emeld--scan-processed 0
                       emeld--last-render-time nil)
                 (emeld--update-header-line)
                 (run-with-timer 0.001 nil #'emeld--process-same-files-worker
                                 buf paths root left right old-gc path-hash)))))))
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
    (emeld--update-header-line)
    (cond
     (emeld--git-mode (emeld--scan-same-git buf))
     (t
      ;; Idempotent: a no-op if the initial scan already cached these roots.
      (emeld--prewarm-gitignore emeld--left-root buf)
      (emeld--prewarm-gitignore emeld--right-root buf)
      (if (eq emeld-same-files-method 'list)
          (emeld--scan-same-list buf)
        (emeld--scan-same-diff buf))))))

(defun emeld--scan-same-diff (buf)
  "Find identical files by re-running `diff -rqs' (see `emeld-same-files-method')."
  (with-current-buffer buf
    (let* ((excludes (emeld--diff-exclude-args))
           (left emeld--left-root)
           (right emeld--right-root)
           (root emeld--root-node)
           (command `("diff" "-rqs" ,@(emeld--whitespace-args) ,@excludes
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

(defun emeld-edit-dir-locals ()
  "Open the project's `.dir-locals.el' to review filter groups by hand.
Opens the file at `emeld--left-root' — the same file
`emeld-add-filter-at-point' persists new filters to.  Edit the
`emeld-filter-groups' value there, save, then rescan (`emeld-rescan')
to apply the change.  Visits a new file when none exists yet; nothing
is written until you save."
  (interactive)
  (unless emeld--left-root (user-error "No comparison loaded"))
  (let ((file (expand-file-name dir-locals-file emeld--left-root)))
    (unless (file-exists-p file)
      (message "No %s yet — saving will create it" (abbreviate-file-name file)))
    (find-file file)))

(defun emeld-add-filter-at-point (glob group)
  "Add GLOB to filter GROUP and persist it to the project `.dir-locals.el'.
Interactively, GLOB defaults to the basename of the node at point — edit it
to a pattern such as `*.log', or a `sub/dir' relative path — and GROUP is
read with completion over the existing `emeld-filter-groups' names; an
unknown name creates a new, active group.

The updated `emeld-filter-groups' value is written to `.dir-locals.el' at
the left root (via `add-dir-local-variable', under the `nil' mode used in
the manual setup, then saved), applied to this comparison, and rescanned
so the matching files drop out immediately.  This lets you permanently
tidy up a directory comparison — excluding build/cache/noise paths —
without hand-editing `.dir-locals.el'."
  (interactive
   (let* ((node (or (get-text-property (point) 'emeld-node)
                    (user-error "Point is not on a file or directory")))
          (base (emeld-node-name node))
          (g (read-string
              (format "Filter glob (basename, *.ext, or sub/dir) [%s]: " base)
              nil nil base))
          (names (mapcar #'car emeld-filter-groups))
          (default-group (or (car names) "Project"))
          (grp (completing-read
                (format "Add to filter group [%s]: " default-group)
                names nil nil nil nil default-group)))
     (list g grp)))
  (setq glob (string-trim glob))
  (when (string-empty-p glob) (user-error "Empty glob"))
  (unless emeld--left-root (user-error "No comparison loaded"))
  (let* ((buf (current-buffer))
         (groups (copy-tree emeld-filter-groups))
         (entry (assoc group groups)))
    (if entry
        ;; Append to an existing group unless the glob is already there.
        (if (member glob (emeld--group-globs entry))
            (message "\"%s\" is already in group \"%s\"" glob group)
          (setcdr (last entry) (list glob)))
      ;; New group: (NAME t GLOB) — active by default.
      (setq groups (append groups (list (list group t glob)))))
    ;; Persist to .dir-locals.el at the left root and save it (no manual edit).
    (let ((default-directory emeld--left-root))
      (save-window-excursion
        (add-dir-local-variable nil 'emeld-filter-groups groups)
        (when (buffer-modified-p) (save-buffer))))
    ;; Apply now: update the buffer-local value, make the group active, rescan.
    (with-current-buffer buf
      (setq-local emeld-filter-groups groups)
      (unless (member group emeld--active-filter-groups)
        (setq emeld--active-filter-groups (cons group emeld--active-filter-groups)))
      (message "Filtering \"%s\" via group \"%s\" — saved to %s"
               glob group (abbreviate-file-name
                           (expand-file-name dir-locals-file emeld--left-root)))
      (emeld-refresh))))

(defun emeld-unhide-all ()
  "Restore every node hidden this session by `emeld-hide-at-point'."
  (interactive)
  (if (null emeld--hidden-paths)
      (message "Nothing is hidden")
    (let ((n (length emeld--hidden-paths)))
      (setq emeld--hidden-paths nil)
      (emeld--render)
      (message "Restored %d hidden item%s" n (if (= n 1) "" "s")))))

(defun emeld-hide-at-point (&optional arg)
  "Temporarily hide the node at point for this session (not persisted).
The node's relative path is remembered buffer-locally and skipped by the
renderer, so it — and, for a directory, its whole subtree — drops out of
the view.  Handy for decluttering a scan you still want intact: hides
survive a rescan within the session but are never written to
`.dir-locals.el'.  For a permanent filter use `emeld-add-filter-at-point'
\(\\[emeld-add-filter-at-point]) instead.

With a prefix ARG (\\[universal-argument] \\[emeld-hide-at-point]),
unhide everything hidden this session (`emeld-unhide-all')."
  (interactive "P")
  (if arg
      (emeld-unhide-all)
    (let ((node (get-text-property (point) 'emeld-node)))
      (unless node (user-error "No node at point"))
      (when (eq node emeld--root-node) (user-error "Cannot hide the root node"))
      (let ((rel (emeld--node-relpath node)))
        (cl-pushnew rel emeld--hidden-paths :test #'equal)
        ;; `emeld--render' falls back to the same line number when point's
        ;; node is gone, so point lands on whatever moves up into its place.
        (emeld--render)
        (message "Hid \"%s\" (%d hidden this session; %s restores all)"
                 rel (length emeld--hidden-paths)
                 (substitute-command-keys
                  "\\[universal-argument] \\[emeld-hide-at-point]"))))))

;;;###autoload
(defun emeld-diff (dir1 dir2 &optional depth)
  "Compare DIR1 and DIR2 side-by-side.
With a prefix argument, limit the initial scan to DEPTH directory levels
\(a bare \\[universal-argument] means depth 1) using a fast level-by-level
scan, leaving deeper and one-sided directories unscanned and marked with a
dim `…'.  Drill into a marked directory with `\\[universal-argument] g'
\(`emeld-refresh-subdir').  Without a prefix, scan fully (the default).
Useful for very large trees or slow filesystems (SMB/NFS) where a full
recursive compare would block on deep I/O."
  (interactive
   (list (read-directory-name "Left directory: ")
         (read-directory-name "Right directory: ")
         (emeld--prefix-to-depth current-prefix-arg)))
  (let ((buf (get-buffer-create (format "*emeld: %s <-> %s*"
                                        (file-name-nondirectory (directory-file-name dir1))
                                        (file-name-nondirectory (directory-file-name dir2))))))
    (with-current-buffer buf
      (emeld-mode)
      (setq default-directory (file-name-as-directory (expand-file-name dir1)))
      (setq emeld--scan-max-depth depth)
      (emeld--ensure-dir-locals default-directory)
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
(defun emeld-diff-dwim (&optional depth)
  "Start emeld-diff guessing directories from visible dired buffers.
Prompts for confirmation or selection.  A prefix argument is passed
through to `emeld-diff' as the scan-depth limit (e.g. `C-u 2' scans two
levels; bare `C-u' scans one), for a fast shallow scan of large or slow
trees."
  (interactive (list (emeld--prefix-to-depth current-prefix-arg)))
  (require 'dired-aux)
  (let* ((target-b (dired-dwim-target-directory))
         (target-a (if (eq major-mode 'dired-mode)
                       default-directory
                     (car (emeld--get-dwim-dirs))))
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
      (emeld-diff left-abs right-abs depth))))

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

;;;; ─── Git utilities (shared by emeld-git.el and the sidebar) ──────

(defconst emeld--git-empty-tree "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
  "SHA-1 of git's canonical empty tree, used as a root commit's parent.")

(defun emeld--git-rev-label (rev)
  "Return a short human label for REV.
REV is a revision string, or the symbol `working' or `staged'.  A full
40-char SHA is abbreviated; a SHA that points at HEAD is shown as
\"HEAD (SHA)\"; the empty tree shows as a root marker."
  (cond ((eq rev 'working) "working")
        ((eq rev 'staged) "index")
        ((and (stringp rev) (string= rev emeld--git-empty-tree)) "∅ (root)")
        ((and (stringp rev) (string-match-p "\\`[0-9a-f]\\{40\\}\\'" rev))
         (if (and emeld--git-head-sha (string= rev emeld--git-head-sha))
             (format "HEAD (%s)" (substring rev 0 8))
           (substring rev 0 8)))
        ((stringp rev) rev)
        (t (format "%s" rev))))

(defun emeld--git-toplevel (dir)
  "Return the git work-tree root containing DIR (trailing slash), or nil."
  (when (and dir (emeld--git-work-tree-p dir))
    (let ((default-directory (file-name-as-directory (expand-file-name dir))))
      (with-temp-buffer
        (when (eq 0 (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
          (let ((top (string-trim (buffer-string))))
            (and (not (string-empty-p top)) (file-name-as-directory top))))))))

(defun emeld--git-rev-args (left right)
  "Return the revision argument list for `git diff' comparing LEFT to RIGHT.
LEFT/RIGHT are revision strings or the symbols `working'/`staged'.  The
returned list is spliced after `git diff [--name-status]' and before any
pathspec, so it must contain only revision selectors."
  (cond
   ((and (memq left '(staged working)) (eq right 'working)) nil)
   ((and (stringp left) (eq right 'staged)) (list "--cached" left))
   ((and (stringp left) (eq right 'working)) (list left))
   ((and (stringp left) (stringp right)) (list left right))
   (t (user-error "Unsupported revision combination: %s vs %s"
                  (emeld--git-rev-label left) (emeld--git-rev-label right)))))

;;;; ─── Faces ───────────────────────────────────────────────────────

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
  '((t :inherit font-lock-function-name-face :weight bold))
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
    (define-key map (kbd "/")   #'emeld-expand-changes)
    (define-key map (kbd "RET") #'emeld-action)
    (define-key map (kbd "e")   #'emeld-ediff)
    (define-key map (kbd "=")   #'emeld-soft-diff)
    (define-key map (kbd "f")   #'emeld-find-file)
    (define-key map (kbd "v")   #'emeld-find-file)
    (define-key map (kbd "A")   #'emeld-dired-left)
    (define-key map (kbd "B")   #'emeld-dired-right)
    (define-key map (kbd "g")   #'emeld-redraw)
    (define-key map (kbd "G")   #'emeld-rescan)
    (define-key map (kbd "k")   #'emeld-stop-scan)
    (define-key map (kbd "R")   #'emeld-toggle-mirror)
    (define-key map (kbd "W")   #'emeld-swap-panes)
    (define-key map (kbd "n")   #'emeld-next-line)
    (define-key map (kbd "p")   #'emeld-previous-line)
    (define-key map (kbd "o")   #'emeld-jump-other-side)
    (define-key map (kbd "P")   #'emeld-format-patch)
    (define-key map (kbd "C")   #'emeld-copy)
    (define-key map (kbd "w")   #'emeld-copy-path-at-point)
    (define-key map (kbd "x")   #'emeld-delete)
    (define-key map (kbd "D")   #'emeld-diff-root)
    (define-key map (kbd "d")   #'emeld-diff-dir)
    (define-key map (kbd "c")   #'emeld-git-toggle-commit-message)
    (define-key map (kbd ",")   #'emeld-git-history-older)
    (define-key map (kbd ".")   #'emeld-git-history-newer)
    (define-key map (kbd "SPC") #'emeld-soft-diff)
    (define-key map (kbd "m")   #'emeld-mark)
    (define-key map (kbd "u")   #'emeld-unmark)
    (define-key map (kbd "U")   #'emeld-unmark-all)
    (define-key map (kbd "* m") #'emeld-mark-modified)
    (define-key map (kbd "* l") #'emeld-mark-left-only)
    (define-key map (kbd "* c") #'emeld-mark-to-copy)
    (define-key map (kbd "* a") #'emeld-mark-all)
    (define-key map (kbd "S")   #'emeld-toggle-show-same)
    (define-key map (kbd "M")   #'emeld-toggle-show-different)
    (define-key map (kbd "N")   #'emeld-toggle-show-added)
    (define-key map (kbd "T")   #'emeld-toggle-tree-lines)
    (define-key map (kbd "z")   #'emeld-cycle-tree-density)
    (define-key map (kbd "s")   #'emeld-toggle-show-strikethrough)
    (define-key map (kbd "I")   #'emeld-toggle-gitignore)
    (define-key map (kbd "i")   #'emeld-toggle-ignore-whitespace)
    (define-key map (kbd "&")   #'emeld-add-filter-at-point)
    (define-key map (kbd "C-k") #'emeld-hide-at-point)
    (define-key map (kbd "F")   #'emeld-toggle-show-files)
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
  (make-local-variable 'emeld-show-files)
  (make-local-variable 'emeld-respect-gitignore)
  (make-local-variable 'emeld-ignore-whitespace)
  ;; Layout tweaks are per-comparison too, so adjusting tree-lines, the
  ;; compact layout, indentation or the column split in one buffer does not
  ;; leak into other emeld buffers or the global defaults.
  (make-local-variable 'emeld-show-tree-lines)
  (make-local-variable 'emeld-tree-density)
  (make-local-variable 'emeld-indent-offset)
  (make-local-variable 'emeld-column-ratio)
  (setq-local revert-buffer-function #'emeld-refresh)
  (add-hook 'kill-buffer-hook #'emeld--stop-scan nil t)
  (emeld--update-header-line))

(defun emeld--effective-column-width ()
  "Return the buffer-local pane column width, or a fresh estimate.
Falls back to a window-derived estimate when `emeld--column-width' has not
yet been set (e.g. before the first render)."
  (or (bound-and-true-p emeld--column-width)
      (/ (* (window-width) emeld-column-ratio) 100)))

(defun emeld--move-to-node-name (on-right)
  "Move point to the node name on the current side (ON-RIGHT if non-nil).
If the current side is empty but the other has a file, snap to it."
  (let* ((width (emeld--effective-column-width))
         (limit (if on-right (line-end-position) (+ (line-beginning-position) width))))
    (if on-right
        (move-to-column width)
      (beginning-of-line))
    
    ;; Skip mark char, prefixes, icons, and spaces on current side
    (let ((start-on-side (point)))
      (while (and (< (point) (line-end-position))
                  (< (point) limit)
                  (looking-at "[]* └├│─[+-]"))
        (forward-char 1))

      ;; If we reached the limit on the left side, back up slightly to stay on the left
      (when (and (not on-right) (>= (current-column) width))
        (goto-char start-on-side)
        (skip-chars-forward "* └├│─")))
    
    ;; Smart snapping: only if this side is truly empty and the other side has
    ;; a file.  Skipped when `emeld-show-strikethrough' is on, since the empty
    ;; side then shows the struck name — a valid place to land and to mark for
    ;; deletion (so e.g. marking down the left column stays on the left).
    (let ((node (get-text-property (point) 'emeld-node)))
      (when (and node (emeld-node-parent node) ;; Only snap for children, not root nodes
                 (not emeld-show-strikethrough))
        (let* ((has-left (emeld-node-left-path node))
               (has-right (emeld-node-right-path node))
               (current-is-empty (not (if on-right has-right has-left))))
          (when (and current-is-empty (if on-right has-left has-right))
            (if on-right (beginning-of-line) (move-to-column width))
            (let ((new-limit (if on-right (+ (line-beginning-position) width) (line-end-position))))
              (while (and (< (point) (line-end-position))
                          (< (point) new-limit)
                          (looking-at "[]* └├│─[+-]"))
                (forward-char 1)))))))))

(defun emeld-next-line ()
  "Move to the next line and align to node name."
  (interactive)
  (let* ((width (emeld--effective-column-width))
         (on-right (>= (current-column) width)))
    (forward-line 1)
    (emeld--move-to-node-name on-right)))

(defun emeld-previous-line ()
  "Move to the previous line and align to node name."
  (interactive)
  (let* ((width (emeld--effective-column-width))
         (on-right (>= (current-column) width)))
    (forward-line -1)
    (emeld--move-to-node-name on-right)))

(defun emeld-jump-other-side ()
  "Jump to the other side of the directory comparison."
  (interactive)
  (let* ((width (emeld--effective-column-width))
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

(defun emeld--tree-density-next (density)
  "Return the density that follows DENSITY in the cycle.
compact → extreme → wide → compact."
  (pcase density
    ('compact 'extreme)
    ('extreme 'wide)
    (_        'compact)))

(defun emeld-cycle-tree-density ()
  "Cycle the tree density: compact → extreme → wide → compact.
See `emeld-tree-density'."
  (interactive)
  (setq emeld-tree-density (emeld--tree-density-next emeld-tree-density))
  (message "Tree density: %s" emeld-tree-density)
  (emeld--render))

(define-obsolete-function-alias 'emeld-toggle-compact-tree
  'emeld-cycle-tree-density "emeld 0.5")

(defun emeld-toggle-mirror ()
  "Toggle `emeld-copy-mirror', rsync `--delete' exact-mirror copying.
When on, `emeld-copy' (\\[emeld-copy]) makes a destination directory
exactly match its source, deleting destination-only files.  The header
line shows `[Mirror]' while it is enabled.  Has no effect unless copies
use rsync (see `emeld-copy-method')."
  (interactive)
  (setq emeld-copy-mirror (not emeld-copy-mirror))
  (force-mode-line-update)
  (message "Copy mirror (rsync --delete): %s" (if emeld-copy-mirror "ON" "off")))

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

(defun emeld-toggle-show-files ()
  "Toggle file visibility to show only the directory hierarchy.
When disabled, only directories are displayed."
  (interactive)
  (setq emeld-show-files (not emeld-show-files))
  (message "Show files: %s" (if emeld-show-files "on" "off"))
  (emeld--render))

(defun emeld-toggle-gitignore ()
  "Toggle whether `.gitignore' rules are respected, then rescan.
Unlike the view toggles this changes which files are scanned, so it
re-runs the comparison rather than just re-rendering."
  (interactive)
  (setq emeld-respect-gitignore (not emeld-respect-gitignore))
  (message "Respect .gitignore: %s" (if emeld-respect-gitignore "on" "off"))
  (emeld-refresh))

(defun emeld-toggle-ignore-whitespace ()
  "Cycle `emeld-ignore-whitespace' (off → amount → all → off), then rescan.
Like `emeld-toggle-gitignore' this changes which files count as differing,
so it re-runs the comparison rather than just re-rendering."
  (interactive)
  (setq emeld-ignore-whitespace
        (pcase emeld-ignore-whitespace
          ('nil 'change)
          ('change 'all)
          (_ nil)))
  (message "Ignore whitespace: %s"
           (pcase emeld-ignore-whitespace
             ('change "amount (-b)")
             ('all "all (-w)")
             (_ "off")))
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

(defun emeld--set-expanded-changes ()
  "Expand directories leading to a change, collapse those that don't.
Mutates the `:expanded' slots under `emeld--root-node' without rendering;
callers redisplay.  A directory is expanded when its `diff-status' is not
`same' — i.e. it contains a modified, added, or removed descendant."
  (when emeld--root-node
    (cl-labels ((walk (node)
                  (when (emeld-node-is-dir node)
                    (setf (emeld-node-expanded node)
                          (not (eq (emeld-node-diff-status node) 'same)))
                    (dolist (child (emeld-node-children node))
                      (walk child)))))
      (dolist (child (emeld-node-children emeld--root-node))
        (walk child)))))

(defun emeld-expand-changes ()
  "Expand directories that lead to a change, collapse those that don't.
A directory is expanded when its `diff-status' is not `same' — i.e. it
contains a modified, added, or removed descendant.  Useful for focusing
the view after \\[emeld-collapse-all], or after toggling same files on."
  (interactive)
  (when emeld--root-node
    (emeld--set-expanded-changes)
    (emeld--render)))

(defun emeld-cycle-subtree ()
  "Cycle the subtree at point through FOLDED → DIRS → SUBTREE, org-mode style.

On a file, TAB instead toggles an inline diff preview beneath the row
\(`emeld-toggle-inline-diff'), a Magit-style quick peek.

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
     ((null node)
      (user-error "No node at point"))
     ;; On a file, TAB peeks an inline diff (Magit-style) instead of cycling.
     ((not (emeld-node-is-dir node))
      (emeld-toggle-inline-diff))
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
  "Most recent state from `emeld-cycle-visibility'.
One of `top', `dirs', `changes', or nil.")

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
  "Cancel the active async scan and clean up processes.
Bumps `emeld--scan-generation' so any in-flight timer-driven work (the
bounded scan, the identical-files merge) sees a newer generation and aborts."
  (cl-incf emeld--scan-generation)
  (when (and emeld--scan-process
             (process-live-p emeld--scan-process))
    (delete-process emeld--scan-process))
  ;; An async rsync copy is independent of the scan, but a rescan or buffer
  ;; kill should not leave it running with a sentinel pointing at a stale tree.
  (when (and emeld--copy-proc (process-live-p emeld--copy-proc))
    (delete-process emeld--copy-proc))
  (dolist (p emeld--gitignore-procs)
    (when (process-live-p p) (delete-process p)))
  (setq emeld--gitignore-procs nil
        emeld--scan-process nil
        emeld--copy-proc nil
        emeld--scan-diff-lines nil
        emeld--scan-buf nil))

(defun emeld-stop-scan ()
  "Abort an in-progress scan, keeping whatever has been built so far.
Cancels the async `diff' process and the timer-driven tree building (the
full scan, the bounded scan and the identical-files merge all check the
scan generation and bail).  The partial tree is left in place; rescan with
\\[emeld-rescan] or scan a single directory with `\\[universal-argument] \
\\[emeld-rescan]'."
  (interactive)
  (if (not (memq emeld--status-phase '(running-diff building-tree)))
      (message "No scan in progress")
    (emeld--stop-scan)
    (when (timerp emeld--chunk-worker-timer)
      (cancel-timer emeld--chunk-worker-timer))
    (setq emeld--chunk-worker-timer nil
          emeld--status-phase 'done)
    (emeld--update-header-line)
    (emeld--render)
    (message "Scan stopped")))

(defun emeld--reset-display-var (var)
  "Reset VAR to the standard value from its `defcustom' definition."
  (let ((std (get var 'standard-value)))
    (when std
      (set var (eval (car std) t)))))

(defun emeld-redraw ()
  "Reset the display layout to defaults and re-render, without rescanning.
Restores the indentation (`emeld-indent-offset'), the pane/tree separation
(`emeld-column-ratio') and the tree density (`emeld-tree-density') to
their standard values, recomputes the column width from the current window
width, and redraws — preserving point and expansion state.  Use this to
undo layout tweaks made with \\[emeld-increase-indent]/\\[emeld-decrease-indent],
\\[emeld-grow-column]/\\[emeld-shrink-column] or \\[emeld-cycle-tree-density],
or after resizing the window or frame.  For a fresh comparison use
`emeld-rescan' (\\[emeld-rescan])."
  (interactive)
  (unless emeld--root-node
    (user-error "No comparison loaded"))
  (emeld--reset-display-var 'emeld-indent-offset)
  (emeld--reset-display-var 'emeld-column-ratio)
  (emeld--reset-display-var 'emeld-tree-density)
  (emeld--render)
  (message "Display reset (indent %d, columns %d/%d, density %s)"
           emeld-indent-offset emeld-column-ratio (- 100 emeld-column-ratio)
           emeld-tree-density))

(defun emeld-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the comparison asynchronously.
Uses `make-process' to run `diff -rq' as an OS-level async process, so
Emacs stays fully responsive during the scan.  When `emeld--scan-max-depth'
is set (a depth-limited `emeld-diff'), re-runs the bounded level-by-level
scan instead, preserving the shallow view.

The optional arguments are ignored; they exist so this can serve as the
buffer's `revert-buffer-function', which is called with two arguments."
  (interactive)
  (emeld--stop-scan)
  (setq emeld--status-phase 'running-diff)
  (let* ((buf (current-buffer))
         (root emeld--root-node)
         (node (get-text-property (point) 'emeld-node)))
    (setq emeld--scan-buf buf
          emeld--benchmark-data nil
          emeld--saved-expanded (emeld--collect-expanded root)
          emeld--saved-dirs (emeld--collect-dirs root)
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
    (cond
     (emeld--git-mode (emeld--start-git-diff buf))
     (emeld--scan-max-depth (emeld--start-bounded-diff buf emeld--scan-max-depth))
     (t (emeld--start-diff buf)))))

(defun emeld--diff-subdir-entries (left-sub right-sub)
  "Diff LEFT-SUB against RIGHT-SUB, returning subdir-relative entries.
Each entry is (REL-PATH . STATUS) with REL-PATH relative to the
subdirectory.  Honors the active filter groups and `.gitignore' via the
same `--exclude' args as a full scan.  Runs `diff -rqs' when
`emeld-show-same' is on so identical files are included, else `diff -rq'.
Synchronous: the diff is scoped to one subtree, so it is bounded."
  (let ((args (append (list (if emeld-show-same "-rqs" "-rq"))
                      (emeld--whitespace-args)
                      (emeld--diff-exclude-args)
                      (list (expand-file-name left-sub)
                            (expand-file-name right-sub))))
        (entries nil))
    (with-temp-buffer
      (apply #'call-process "diff" nil t nil args)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((parsed (emeld--parse-diff-line
                       (buffer-substring-no-properties (line-beginning-position)
                                                       (line-end-position))
                       left-sub right-sub)))
          (when parsed (push parsed entries)))
        (forward-line 1)))
    (nreverse entries)))

(defun emeld-refresh-subdir ()
  "Rescan only the directory at point, merging the result into the tree.
Bound to \\`C-u G' (see `emeld-rescan').  Useful when you know a single
subdirectory changed on disk and want to avoid a full rescan
(\\[emeld-rescan]); the rest of the tree is left untouched.  If point is
on a file, its containing directory is rescanned."
  (interactive)
  (when emeld--git-mode
    (user-error "Subdirectory rescan is not used in git mode; press `G' to rescan"))
  (unless emeld--root-node (user-error "No comparison loaded"))
  (let* ((node0 (get-text-property (point) 'emeld-node))
         (node (cond ((null node0) (user-error "No node at point"))
                     ((emeld-node-is-dir node0) node0)
                     (t (emeld-node-parent node0)))))
    (when (or (null node) (eq node emeld--root-node))
      (user-error "Use `G' to rescan the whole comparison"))
    (let* ((rel (emeld--node-relpath node))
           (left-sub (concat (file-name-as-directory emeld--left-root) rel))
           (right-sub (concat (file-name-as-directory emeld--right-root) rel))
           (left-exists (file-directory-p left-sub))
           (right-exists (file-directory-p right-sub)))
      (cond
       ;; Gone on both sides — drop the node from its parent.
       ((not (or left-exists right-exists))
        (let ((parent (emeld-node-parent node)))
          (setf (emeld-node-children parent) (delq node (emeld-node-children parent))))
        (emeld--render)
        (message "%s no longer exists on either side; removed" rel))
       ;; Present on both sides — re-diff and rebuild the subtree.
       ((and left-exists right-exists)
        (let ((entries (emeld--diff-subdir-entries left-sub right-sub))
              (saved (emeld--collect-expanded node))
              (saved-dirs (emeld--collect-dirs node)))
          (setf (emeld-node-left-path node) left-sub
                (emeld-node-right-path node) right-sub
                (emeld-node-children node) (emeld--entries-to-nodes entries rel node))
          (setf (emeld-node-diff-status node)
                (if (cl-some (lambda (c) (not (eq (emeld-node-diff-status c) 'same)))
                             (emeld-node-children node))
                    'different 'same))
          ;; A full recursive subdir scan clears any depth-limit boundary mark.
          (setf (emeld-node-scanned node) t)
          (let ((emeld--saved-expanded saved)
                (emeld--saved-dirs saved-dirs))
            (emeld--restore-expanded node))
          (emeld--render)
          (message "Rescanned %s — %d change%s" rel
                   (cl-count-if (lambda (e) (not (eq (cdr e) 'same))) entries)
                   (if (= 1 (cl-count-if (lambda (e) (not (eq (cdr e) 'same))) entries))
                       "" "s"))))
       ;; Single-sided now — re-enumerate the side that exists.
       (t
        (setf (emeld-node-diff-status node) (if left-exists 'left-only 'right-only)
              (emeld-node-left-path node) (and left-exists left-sub)
              (emeld-node-right-path node) (and right-exists right-sub)
              (emeld-node-children node) nil)
        (setf (emeld-node-scanned node) t)
        (emeld--maybe-populate-lazy-dir node)
        (emeld--render)
        (message "Rescanned %s — only on the %s side" rel
                 (if left-exists "left" "right")))))))

(defun emeld-rescan (&optional arg)
  "Rescan the comparison.
With a prefix ARG (\\[universal-argument]), rescan only the directory at
point (`emeld-refresh-subdir'); otherwise rescan the whole comparison
asynchronously (`emeld-refresh')."
  (interactive "P")
  (if arg
      (emeld-refresh-subdir)
    (emeld-refresh)))

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
  "Restore expansion state from `emeld--saved-expanded' for tree under NODE.
Only directories that already existed when the state was captured (those in
`emeld--saved-dirs') are re-collapsed when absent from `emeld--saved-expanded'.
Directories that have newly appeared since — e.g. a subtree revealed by
toggling `emeld-respect-gitignore' off — keep their default-expanded state,
so the new content is shown rather than silently folded (matching a first
scan, where everything starts expanded)."
  (when emeld--saved-expanded
    (dolist (child (emeld-node-children node))
      (when (emeld-node-is-dir child)
        (let ((rel (emeld--node-relpath child)))
          (when (and (member rel emeld--saved-dirs)
                     (not (member rel emeld--saved-expanded)))
            (setf (emeld-node-expanded child) nil))
          (emeld--restore-expanded child))))))

(defun emeld--collect-dirs (node)
  "Return a list of relative paths for every directory under NODE."
  (let ((paths nil))
    (dolist (child (emeld-node-children node))
      (when (emeld-node-is-dir child)
        (push (emeld--node-relpath child) paths)
        (setq paths (nconc (emeld--collect-dirs child) paths))))
    paths))

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
  "Prompt for a new root directory for side ON-LEFT and update the buffer.
In git mode this re-selects the revision for that side instead of a directory."
  (if emeld--git-mode
      (let ((rev (emeld--read-git-rev
                  (format "%s revision: " (if on-left "Left (old)" "Right (new)"))
                  emeld--git-repo
                  (if on-left emeld--git-left-rev emeld--git-right-rev))))
        (if on-left
            (setq emeld--git-left-rev rev)
          (setq emeld--git-right-rev rev))
        (setf (emeld-node-name emeld--root-node)
              (emeld--git-rev-label emeld--git-left-rev)
              (emeld-node-name-right emeld--root-node)
              (emeld--git-rev-label emeld--git-right-rev))
        (emeld-refresh))
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
      (emeld-refresh)))))

(defun emeld-action ()
  "Perform default action on node (ediff or view).

On the header line (directory paths), prompt to replace the
current side's directory with a new one via completion.
On files, run ediff or open.  On directories, open in dired."
  (interactive)
  (let* ((node (get-text-property (point) 'emeld-node))
         (header (get-text-property (point) 'emeld-header))
         (commit (get-text-property (point) 'emeld-git-commit)))
    (cond
     (commit (emeld-git-toggle-commit-message))
     (header
      (let ((cur-col (current-column)))
        (emeld--read-new-root (< cur-col emeld--column-width))))
     (node
      (let* ((cur-col (current-column))
             (on-left (< cur-col emeld--column-width)))
        (if emeld--git-mode
            (emeld--git-action node on-left)
          (let* ((left (emeld-node-left-path node))
                 (right (emeld-node-right-path node))
                 (side-path (if on-left left right)))
            (cond ((and left right (not (emeld-node-is-dir node)))
                   (if (eq (emeld-node-diff-status node) 'same)
                       (find-file-other-window side-path)
                     (ediff-files left right)))
                  (side-path (find-file-other-window side-path))
                  (t (message "No file on %s side"
                              (if on-left "left" "right")))))))))))

(defun emeld-find-file ()
  "Open the file on the current side in another window.
In git mode the file is shown at the current side's revision (via
`git show'); the working-tree side opens the on-disk file."
  (interactive)
  (let* ((node (get-text-property (point) 'emeld-node))
         (on-left (< (current-column) emeld--column-width)))
    (when node
      (if emeld--git-mode
          (cond
           ((emeld-node-is-dir node)
            (message "Cannot open a directory at a revision; use RET to expand it"))
           ((if on-left (emeld-node-left-path node) (emeld-node-right-path node))
            (switch-to-buffer-other-window
             (emeld--git-blob-buffer (if on-left emeld--git-left-rev emeld--git-right-rev)
                                     (emeld--node-relpath node))))
           (t (message "No file on %s side" (if on-left "left" "right"))))
        (let ((path (if on-left (emeld-node-left-path node) (emeld-node-right-path node))))
          (if path
              (find-file-other-window path)
            (message "No file on %s side" (if on-left "left" "right"))))))))

(defun emeld--dired-side (on-left)
  "Open dired for the ON-LEFT (else right) side as a quick jump.
Targets the node at point on that side: a directory opens dired in it, a
file opens dired on its parent with point on the file.  When the node has
no path on that side (or point is not on a node) the side's comparison
root is used instead.  In git mode the node's repo-relative path is
resolved against the repo's working tree."
  (let* ((node (get-text-property (point) 'emeld-node))
         (root (if on-left emeld--left-root emeld--right-root))
         (side-path
          (cond
           ((and node emeld--git-mode emeld--git-repo)
            (expand-file-name (emeld--node-relpath node) emeld--git-repo))
           (node (if on-left (emeld-node-left-path node)
                   (emeld-node-right-path node)))))
         (target (or (and side-path (file-exists-p side-path) side-path)
                     root)))
    (unless (and target (file-exists-p target))
      (user-error "No %s directory to open" (if on-left "left" "right")))
    (select-window (split-window-right))
    (if (file-directory-p target)
        (dired target)
      (dired (file-name-directory target))
      (dired-goto-file target))))

(defun emeld-dired-left ()
  "Jump to dired for the left side of the node at point (or the left root)."
  (interactive)
  (emeld--dired-side t))

(defun emeld-dired-right ()
  "Jump to dired for the right side of the node at point (or the right root)."
  (interactive)
  (emeld--dired-side nil))

(defun emeld-soft-diff ()
  "Contextual \"just act\" command for SPC (and `=').
Do the obvious thing for the node at point:

- on a directory, toggle its expansion;
- on a file that differs between the two sides, show a quick `diff';
- on a file that exists on only one side (new), or is identical on
  both, show it in another window (preferring the side at point).

Unlike \\[emeld-action], this is a preview: the diff or file is only
displayed, never selected, so point stays in the tree buffer.  Combined
with \\[emeld-next-line]/\\[emeld-previous-line] to step and
\\[emeld-jump-other-side] to switch sides, SPC lets you walk the tree and
preview each node without leaving it."
  (interactive)
  (let ((node (get-text-property (point) 'emeld-node)))
    (if (not node)
        (message "No node at point")
      (if (emeld-node-is-dir node)
          (emeld-toggle-expand)
        (if emeld--git-mode
            (emeld--git-soft-diff node)
        (let ((left (emeld-node-left-path node))
              (right (emeld-node-right-path node)))
          ;; Display, never select: keep the tree window current so SPC can
          ;; preview successive nodes.  `diff' already uses `display-buffer';
          ;; the file branch uses `find-file-noselect' + `display-buffer'
          ;; instead of `find-file-other-window' for the same reason, and the
          ;; `save-selected-window' guards against any window the displays or
          ;; their hooks might otherwise select.
          (save-selected-window
            (cond
             ;; Modified: present on both sides and differing -> quick diff.
             ((and left right
                   (not (file-directory-p left))
                   (not (file-directory-p right))
                   (eq (emeld-node-diff-status node) 'different))
              (diff right left))
             ;; New (one side only) or identical -> show the side at point.
             ((or left right)
              (let* ((side (cdr (emeld--node-side-at-point)))
                     (path (if (eq side 'left) left right)))
                (display-buffer (find-file-noselect (or path left right)))))
             (t (message "No file at point"))))))))))

(defun emeld--inline-diff-text (node)
  "Return the raw unified-diff text for file NODE, or nil when there is none.
In git mode the diff comes from `git diff' between the two revisions; in
directory mode from `diff -u' (left→right), reusing `emeld--write-patch-hunk'.
Identical files have no diff."
  (cond
   (emeld--git-mode
    ;; Capture the buffer-local repo/revs *before* `with-temp-buffer', where
    ;; those locals would read as nil.
    (let* ((repo emeld--git-repo)
           (rel (emeld--node-relpath node))
           (revargs (emeld--git-rev-args emeld--git-left-rev emeld--git-right-rev))
           (args (append (list "diff" "--no-color") (emeld--whitespace-args)
                         revargs (list "--" rel))))
      (with-temp-buffer
        (let ((default-directory repo))
          (apply #'process-file "git" nil t nil args))
        (buffer-string))))
   ((eq (emeld-node-diff-status node) 'same) nil)
   (t (emeld--write-patch-hunk node))))

(defun emeld--inline-diff-faceify (text)
  "Split unified-diff TEXT into a list of propertized lines for inline display.
Each line gets the appropriate `diff-*' face so themes apply automatically."
  (mapcar
   (lambda (line)
     (let ((face (cond
                  ((string-prefix-p "@@" line) 'diff-hunk-header)
                  ((string-prefix-p "+++" line) 'diff-file-header)
                  ((string-prefix-p "---" line) 'diff-file-header)
                  ((string-prefix-p "diff " line) 'diff-header)
                  ((string-prefix-p "index " line) 'diff-header)
                  ((string-prefix-p "new file" line) 'diff-header)
                  ((string-prefix-p "deleted file" line) 'diff-header)
                  ((string-prefix-p "+" line) 'diff-added)
                  ((string-prefix-p "-" line) 'diff-removed)
                  (t 'diff-context))))
       (propertize line 'face face)))
   (split-string (string-trim-right text "[\n\r]+") "\n")))

(defun emeld--inline-diff-lines (node)
  "Return propertized diff lines for file NODE, or nil when there is nothing."
  (let ((text (emeld--inline-diff-text node)))
    (and text
         (not (string-empty-p (string-trim text)))
         (emeld--inline-diff-faceify text))))

(defun emeld-toggle-inline-diff ()
  "Toggle an inline diff preview of the file node at point.
On a file that differs (or exists on only one side), expand a quick diff
beneath the row — a Magit-style peek that keeps point in the tree; invoke
again to collapse.  Reports a message on a directory or an identical file."
  (interactive)
  (let ((node (get-text-property (point) 'emeld-node)))
    (cond
     ((null node) (message "No node at point"))
     ((emeld-node-is-dir node)
      (message "Inline diff needs a file; TAB cycles a directory"))
     ((emeld-node-inline-diff node)
      (setf (emeld-node-inline-diff node) nil)
      (let ((pos (point))) (emeld--render) (goto-char pos)))
     (t
      (let ((lines (emeld--inline-diff-lines node)))
        (if (null lines)
            (message "Nothing to preview (identical?)")
          (setf (emeld-node-inline-diff node) lines)
          (let ((pos (point))) (emeld--render) (goto-char pos))))))))

(defun emeld-ediff ()
  "Run Ediff on the file at point, comparing the left and right sides.
Requires a file present on both sides.  On a directory, or when only
one side exists, report what is missing (use RET to open a directory)."
  (interactive)
  (let ((node (get-text-property (point) 'emeld-node)))
    (cond
     ((null node) (message "No node at point"))
     ((emeld-node-is-dir node)
      (message "Ediff needs a file; use RET to open the directory"))
     (emeld--git-mode
      (let ((left (emeld-node-left-path node))
            (right (emeld-node-right-path node))
            (rel (emeld--node-relpath node)))
        (if (and left right)
            (ediff-buffers (emeld--git-blob-buffer emeld--git-left-rev rel)
                           (emeld--git-blob-buffer emeld--git-right-rev rel))
          (message "Ediff needs the file on both revisions"))))
     (t
      (let ((left (emeld-node-left-path node))
            (right (emeld-node-right-path node)))
        (if (and left right)
            (ediff-files left right)
          (message "Ediff needs a file on both sides")))))))

(defun emeld--diff-dirs (left right what)
  "Show a recursive unified `diff' of directories RIGHT vs LEFT.
The orientation matches `emeld-soft-diff' (right is OLD, left is NEW),
and the active filter groups and `.gitignore' settings are honoured via
`--exclude', so the diff covers the same files as the comparison tree.
WHAT labels the directory in the echo-area message.

A directory present on only one side has nothing to compare; its node may
even carry a fabricated path for the missing side (intermediate nodes are
synthesised with both paths during the scan), so confirm both sides exist
on disk before diffing rather than running a diff that would just error."
  (let ((have-left (and left (file-directory-p left)))
        (have-right (and right (file-directory-p right))))
    (cond
     ((and have-left have-right)
      ;; `diff' (via `diff-no-select') runs the command through the user's
      ;; shell but, unlike the file arguments, does not quote the switches.
      ;; The `--exclude=GLOB' patterns would otherwise reach the shell
      ;; unquoted and be glob-expanded against the cwd (fish even errors on no
      ;; match), so quote each switch to pass the literal pattern through.
      (let ((switches (mapcar #'shell-quote-argument
                              (append '("-r" "-u") (emeld--diff-exclude-args)))))
        (diff (directory-file-name right) (directory-file-name left) switches)
        (message "Diff of %s" what)))
     ((or have-left have-right)
      (message "%s exists on only one side — nothing to diff" what))
     (t
      (message "No directory to diff (%s)" what)))))

(defun emeld-diff-root ()
  "Show a recursive unified `diff' of the two root directories.
Differences appear in a `*Diff*' buffer; filter groups and `.gitignore'
settings are honoured, matching the comparison tree.  See also
`emeld-diff-dir' for the directory at point."
  (interactive)
  (if emeld--git-mode
      (emeld--git-diff-show nil "root")
    (progn
      (unless (and emeld--left-root emeld--right-root)
        (user-error "No comparison loaded"))
      (emeld--diff-dirs emeld--left-root emeld--right-root "root"))))

(defun emeld-diff-dir ()
  "Show a recursive unified `diff' of the directory at point.
If point is on a file, its containing directory is used; above any
subdirectory this falls back to the root (so it matches
`emeld-diff-root').  Differences appear in a `*Diff*' buffer."
  (interactive)
  (let ((node (get-text-property (point) 'emeld-node)))
    (while (and node (not (emeld-node-is-dir node)))
      (setq node (emeld-node-parent node)))
    (if emeld--git-mode
        (let ((rel (and node (not (eq node emeld--root-node))
                        (emeld--node-relpath node))))
          (emeld--git-diff-show (and rel (not (string-empty-p rel)) rel)
                                (if (and node (not (eq node emeld--root-node)))
                                    (emeld-node-name node)
                                  "root")))
      (let ((dir (or node emeld--root-node)))
        (unless dir (user-error "No comparison loaded"))
        (emeld--diff-dirs (emeld-node-left-path dir)
                          (emeld-node-right-path dir)
                          (emeld-node-name dir))))))

(defun emeld--marked-sides (rel)
  "Return the list of marked sides (subset of (left right)) for REL."
  (and emeld--marked-paths (gethash rel emeld--marked-paths)))

(defun emeld--marked-side-p (rel side)
  "Return non-nil if REL is marked on SIDE (`left' or `right')."
  (memq side (emeld--marked-sides rel)))

(defun emeld--set-mark (rel side on)
  "Mark (ON non-nil) or unmark REL on SIDE in `emeld--marked-paths'.
Drops the entry entirely when no sides remain marked."
  (let ((sides (if on (cl-adjoin side (emeld--marked-sides rel))
                 (delq side (copy-sequence (emeld--marked-sides rel))))))
    (if sides
        (puthash rel sides emeld--marked-paths)
      (remhash rel emeld--marked-paths))))

(defun emeld--update-line-mark (node side marked)
  "Rewrite NODE's SIDE mark glyph on the current line in place.
SIDE is `left' or `right'; MARKED non-nil shows `*', nil clears it.  Locates
the cell by its `emeld-mark-cell' text property (set in `emeld--render-nodes'),
so it is robust to column width and wide-character padding.  Lets a single
mark/unmark avoid a full `emeld--render' of the whole tree."
  (save-excursion
    (beginning-of-line)
    (let ((end (line-end-position)) (pos nil))
      (while (and (not pos) (< (point) end))
        (if (eq (get-text-property (point) 'emeld-mark-cell) side)
            (setq pos (point))
          (goto-char (or (next-single-property-change (point) 'emeld-mark-cell nil end)
                         end))))
      (when pos
        (let ((inhibit-read-only t))
          (delete-region pos (1+ pos))
          (goto-char pos)
          (insert (propertize (if marked "*" " ")
                              'face (and marked 'font-lock-keyword-face)
                              'emeld-mark-cell side
                              'emeld-node node
                              'mouse-face 'highlight)))))))

(defun emeld--node-side-at-point ()
  "Return (NODE . SIDE) for the file at point, SIDE being `left' or `right'.
The side is the column point is in.  When that side has no file: if the
other side does and its absence is displayed here as a strikethrough
\(`emeld-show-strikethrough'), the empty side under point is returned, so it
can be marked to delete the file from the other side (the \"make the other
side match the side you marked\" sync of `emeld-copy').  Otherwise the
other (file) side is returned so the mark sits next to a file.  Signals a
`user-error' if there is no node at point."
  (let ((node (get-text-property (point) 'emeld-node)))
    (unless node (user-error "No node at point"))
    (let* ((on-left (< (current-column) emeld--column-width))
           (this (if on-left 'left 'right))
           (this-path (if on-left (emeld-node-left-path node)
                        (emeld-node-right-path node)))
           (other-path (if on-left (emeld-node-right-path node)
                         (emeld-node-left-path node))))
      (cons node
            (cond (this-path this)
                  ;; Empty side: a visible strikethrough makes it a real
                  ;; target to mark for deletion; else snap to the file side.
                  ((and other-path emeld-show-strikethrough) this)
                  (other-path (if on-left 'right 'left))
                  (t this))))))

(defun emeld-mark ()
  "Mark the file at point on the current side (Dired-style).
The side is determined by which column point is in.  Point then advances to
the next node, staying on the same side where possible."
  (interactive)
  (let* ((ns (emeld--node-side-at-point))
         (side (cdr ns)))
    (emeld--set-mark (emeld--node-relpath (car ns)) side t)
    ;; Update just this line's glyph rather than re-rendering the whole tree.
    (emeld--update-line-mark (car ns) side t)
    (forward-line 1)
    (emeld--move-to-node-name (eq side 'right))))

(defun emeld-unmark ()
  "Unmark the file at point on the current side.
Point then advances to the next node, staying on the same side where possible."
  (interactive)
  (let* ((ns (emeld--node-side-at-point))
         (side (cdr ns)))
    (emeld--set-mark (emeld--node-relpath (car ns)) side nil)
    ;; Update just this line's glyph rather than re-rendering the whole tree.
    (emeld--update-line-mark (car ns) side nil)
    (forward-line 1)
    (emeld--move-to-node-name (eq side 'right))))

(defun emeld-unmark-all ()
  "Unmark all nodes."
  (interactive)
  (clrhash emeld--marked-paths)
  (emeld--render)
  (message "Marks cleared"))

(defun emeld--mark-nodes-by-status (statuses &optional side)
  "Mark on SIDE every file node whose `diff-status' is in STATUSES.
SIDE defaults to `left' (the source side for the left→right sync workflow).
Statuses with a left file are marked for copying; `right-only' nodes have
no left file, so marking them on `left' (the empty side) flags them for
deletion from the right by `emeld-copy' — completing a left→right mirror.
Adds to any existing marks and returns the number newly marked."
  (let ((side (or side 'left)) (count 0))
    (cl-labels ((walk (nodes)
                  (dolist (node nodes)
                    (if (emeld-node-is-dir node)
                        (walk (emeld-node-children node))
                      (when (memq (emeld-node-diff-status node) statuses)
                        (emeld--set-mark (emeld--node-relpath node) side t)
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
  "Mark everything for a left→right mirror: modified, left-only and right-only.
With these marked, `emeld-copy' (\\[emeld-copy]) makes the right directory
exactly match the left: modified files are overwritten, left-only files
are created, and right-only files are deleted (they are marked on the
empty left side, which `emeld-copy' reads as \"delete from the right\")."
  (interactive)
  (emeld--mark-and-report '(different left-only right-only) "file to sync"))

(defun emeld-mark-all ()
  "Mark all changed files for a left→right mirror sync.
Modified, left-only and right-only files; the marked set can then be
applied with `emeld-copy' to make the right match the left (copies plus
deletions of right-only files).  Same file set as `emeld-mark-to-copy'."
  (interactive)
  (emeld--mark-and-report '(different left-only right-only) "changed file"))

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

(defun emeld--copy-use-rsync-p ()
  "Return the rsync program path when copies should go through rsync, else nil.
Honours `emeld-copy-method'; for `auto'/`rsync' this is `executable-find'
of `emeld-rsync-command'.  Does not signal: the strict `rsync' case is
checked once up front in `emeld-copy'."
  (and (memq emeld-copy-method '(auto rsync))
       (executable-find emeld-rsync-command)))

(defun emeld--copy-dest (source dest-root)
  "Return the destination path for SOURCE under DEST-ROOT, making its parent.
The path within the side is preserved; a stray file occupying the parent
directory slot is removed first.  Run in the emeld buffer (reads the
buffer-local roots)."
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
    dest))

(defun emeld--rsync-args (source dest)
  "Return the rsync argument list to copy SOURCE to DEST.
A trailing slash is added to a directory SOURCE/DEST so the contents
mirror into DEST (matching `copy-directory' with COPY-CONTENTS) rather
than nesting SOURCE inside DEST.  `--delete' is appended for a directory
when `emeld-copy-mirror' is enabled, making DEST exactly match SOURCE."
  (let* ((dir (file-directory-p source))
         (src (if dir (file-name-as-directory source) source))
         (dst (if dir (file-name-as-directory dest) dest)))
    (append emeld-rsync-options
            (and dir emeld-copy-mirror '("--delete"))
            (list "--" src dst))))

(defun emeld--copy-one (source dest-root)
  "Copy a single file or directory at SOURCE to DEST-ROOT preserving path.
Synchronous built-in copy (`copy-file' / `copy-directory'); rsync copies
go through `emeld--copy-async' instead.  Creates missing parent
directories."
  (let ((dest (emeld--copy-dest source dest-root)))
    (if (file-directory-p source)
        (if (fboundp 'copy-directory-contents)
            (copy-directory source dest t t t)
          (when (and (file-exists-p dest) (not (file-directory-p dest)))
            (delete-file dest))
          (copy-directory source dest t t))
      (copy-file source dest t))
    (message "Copied %s" (file-name-nondirectory (directory-file-name source)))))

(defun emeld--find-node (rel)
  "Return the tree node whose relative path is REL, or nil.
Descends from `emeld--root-node' matching each path component against
child node names."
  (let ((node emeld--root-node))
    (catch 'done
      (dolist (part (split-string rel "/" t) node)
        (setq node (cl-find part (emeld-node-children node)
                            :key #'emeld-node-name :test #'string=))
        (unless node (throw 'done nil))))))

(defun emeld--rel-from-abs (abs)
  "Return ABS relative to whichever comparison root contains it, else nil."
  (let ((abs (expand-file-name abs))
        (l (file-name-as-directory (expand-file-name emeld--left-root)))
        (r (file-name-as-directory (expand-file-name emeld--right-root))))
    (cond ((string-prefix-p l abs) (file-relative-name abs l))
          ((string-prefix-p r abs) (file-relative-name abs r)))))

(defun emeld--recompute-ancestors (node)
  "Recompute the diff-status of NODE's ancestors up to (and incl.) the root.
A directory becomes `same' only when none of its children differ."
  (let ((p (emeld-node-parent node)))
    (while p
      (setf (emeld-node-diff-status p)
            (if (cl-some (lambda (c) (not (eq (emeld-node-diff-status c) 'same)))
                         (emeld-node-children p))
                'different 'same))
      (setq p (emeld-node-parent p)))))

(defun emeld--reclassify-copied-node (node)
  "Re-classify NODE in place after a copy populated the destination side.
A file is re-statused from the two sides' on-disk presence (a copy makes
both sides identical -> `same'); a directory is re-diffed so a non-mirror
merge that leaves destination-only files is reflected accurately.  NODE's
ancestors are then recomputed.  Mutates the tree without rendering."
  (let* ((rel (emeld--node-relpath node))
         (left-sub (concat (file-name-as-directory emeld--left-root) rel))
         (right-sub (concat (file-name-as-directory emeld--right-root) rel))
         (left-exists (file-exists-p left-sub))
         (right-exists (file-exists-p right-sub)))
    (cond
     ;; Should not happen after a copy, but stay correct: gone on both sides.
     ((not (or left-exists right-exists))
      (let ((parent (emeld-node-parent node)))
        (when parent
          (setf (emeld-node-children parent)
                (delq node (emeld-node-children parent))))))
     ;; Directory now on both sides — re-diff the subtree (handles merges).
     ((and (emeld-node-is-dir node) left-exists right-exists)
      (let ((entries (emeld--diff-subdir-entries left-sub right-sub))
            (saved (emeld--collect-expanded node))
            (saved-dirs (emeld--collect-dirs node)))
        (setf (emeld-node-left-path node) left-sub
              (emeld-node-right-path node) right-sub
              (emeld-node-children node) (emeld--entries-to-nodes entries rel node)
              (emeld-node-scanned node) t)
        (setf (emeld-node-diff-status node)
              (if (cl-some (lambda (c) (not (eq (emeld-node-diff-status c) 'same)))
                           (emeld-node-children node))
                  'different 'same))
        (let ((emeld--saved-expanded saved)
              (emeld--saved-dirs saved-dirs))
          (emeld--restore-expanded node))))
     ;; File (or a directory still single-sided) — re-status from presence.
     (t
      (setf (emeld-node-left-path node) (and left-exists left-sub)
            (emeld-node-right-path node) (and right-exists right-sub)
            (emeld-node-diff-status node)
            (cond ((and left-exists right-exists) 'same)
                  (left-exists 'left-only)
                  (t 'right-only)))))
    (emeld--recompute-ancestors node)))

(defun emeld--update-after-copy (rels)
  "Update only the synced RELS in the tree, then re-render once.
RELS are the relative paths `emeld-copy' acted on (copied or deleted).
Each node is re-classified in place from its current on-disk state — a
copy makes it `same', a deletion drops it from the tree — and ancestors
are recomputed, so the tree reflects the sync without the flash of a full
rescan.  Falls back to a full `emeld-refresh' if any node cannot be
located in the tree."
  (let ((ok t))
    (dolist (rel rels)
      (let ((node (and rel (emeld--find-node rel))))
        (if node
            (emeld--reclassify-copied-node node)
          (setq ok nil))))
    (if ok (emeld--render) (emeld-refresh))))

(defun emeld--copy-async (jobs rels)
  "Copy JOBS via rsync, one at a time, asynchronously.
JOBS is a list of (SOURCE . DEST-ROOT).  RELS are the relative paths the
overall sync touched (copies and any deletions already performed); on
completion only those nodes are updated in place (`emeld--update-after-copy'),
marks are cleared and a summary is reported.  rsync output is logged to
the hidden buffer ` *emeld-rsync*'."
  (when (and emeld--copy-proc (process-live-p emeld--copy-proc))
    (user-error "A copy is already in progress"))
  (let* ((buf (current-buffer))
         (prog (executable-find emeld-rsync-command))
         (log (get-buffer-create " *emeld-rsync*"))
         (total (length jobs))
         (done 0) (count 0) (errors 0)
         run-next)
    (with-current-buffer log (erase-buffer))
    (setq run-next
          (lambda (remaining)
            (cond
             ((not (buffer-live-p buf)) nil) ; emeld buffer gone; abandon quietly
             ((null remaining)
              (with-current-buffer buf
                (setq emeld--copy-proc nil)
                (clrhash emeld--marked-paths)
                (emeld--update-after-copy rels))
              (message "Copied %d/%d node(s) via rsync%s" count total
                       (if (> errors 0)
                           (format " (%d error%s — see ` *emeld-rsync*')"
                                   errors (if (= errors 1) "" "s"))
                         "")))
             (t
              (let ((src (caar remaining))
                    (dst-root (cdar remaining)))
                (cl-incf done)
                (if (not (file-exists-p src))
                    (progn (cl-incf errors) (funcall run-next (cdr remaining)))
                  (condition-case _err
                      (let* ((dest (with-current-buffer buf (emeld--copy-dest src dst-root)))
                             (proc (make-process
                                    :name "emeld-rsync"
                                    :buffer log
                                    :noquery t
                                    :connection-type 'pipe
                                    :command (cons prog (emeld--rsync-args src dest))
                                    :sentinel
                                    (lambda (p _event)
                                      (when (memq (process-status p) '(exit signal))
                                        (if (eq (process-exit-status p) 0)
                                            (cl-incf count)
                                          (cl-incf errors))
                                        (funcall run-next (cdr remaining)))))))
                        (with-current-buffer buf (setq emeld--copy-proc proc))
                        (message "Copying %d/%d: %s" done total
                                 (file-name-nondirectory (directory-file-name src))))
                    (error (cl-incf errors)
                           (funcall run-next (cdr remaining))))))))))
    (message "Copying %d node(s) via rsync..." total)
    (funcall run-next jobs)))

(defun emeld--copy-sync (jobs rels)
  "Copy JOBS (list of (SOURCE . DEST-ROOT)) synchronously with the built-in copy.
RELS are the relative paths the overall sync touched; only those nodes
are updated in place afterwards (`emeld--update-after-copy')."
  (let ((count 0) (errors 0))
    (dolist (job jobs)
      (if (file-exists-p (car job))
          (condition-case nil
              (progn (emeld--copy-one (car job) (cdr job)) (cl-incf count))
            (error (cl-incf errors)))
        (cl-incf errors)))
    (clrhash emeld--marked-paths)
    (emeld--update-after-copy rels)
    (message "Copied %d node(s)%s" count
             (if (> errors 0) (format " (%d errors)" errors) ""))))

(defun emeld--copy-jobs ()
  "Return sync jobs for the marked set (or the node at point).
The rule is \"make the other side match the side you marked\".  Each job
is one of:

  (SOURCE . DEST-ROOT)  copy SOURCE to the other side (the marked side
                        has the file);
  (:delete . PATH)      delete PATH from the other side (the marked side
                        is empty — the struck-through, one-sided case).

From the marked set if any (each acting on its marked side; a both-marked
node uses the left side), otherwise the single node at point on the
current side.  Signals a `user-error' when there is nothing to do."
  (let ((marked (let (paths)
                  (maphash (lambda (k _) (push k paths)) emeld--marked-paths)
                  (nreverse paths))))
    (if marked
        (mapcar (lambda (rel)
                  (let* ((on-left (emeld--marked-side-p rel 'left))
                         (src-root (if on-left emeld--left-root emeld--right-root))
                         (dst-root (if on-left emeld--right-root emeld--left-root))
                         (src (expand-file-name rel src-root)))
                    (if (file-exists-p src)
                        (cons src dst-root)
                      (cons :delete (expand-file-name rel dst-root)))))
                marked)
      (let* ((ns (emeld--node-side-at-point))
             (node (car ns))
             (on-left (eq (cdr ns) 'left))
             (source (if on-left (emeld-node-left-path node) (emeld-node-right-path node)))
             (other  (if on-left (emeld-node-right-path node) (emeld-node-left-path node)))
             (dst-root (if on-left emeld--right-root emeld--left-root)))
        (cond
         (source (list (cons source dst-root)))
         (other  (list (cons :delete other)))
         (t (user-error "No file on either side to sync")))))))

(defun emeld-copy ()
  "Sync marked files/dirs to the other side (or the node at point).
The rule is \"make the other side match the side you marked\": a node
marked on the side that HAS the file is copied across, while a node
marked on its empty (struck-through) side is DELETED from the other side.
With no marks the node at point is synced from the side point is in.
Missing parent directories are created on the destination.

Copies use rsync or the built-in copy per `emeld-copy-method'; rsync
copies run asynchronously (one node at a time) so Emacs stays responsive,
built-in copies are synchronous.  When `emeld-copy-mirror' is on, rsync
directory copies use `--delete' to make the destination an exact mirror.
Deletions move the file to trash and run synchronously first.

On completion only the touched nodes are updated in place (re-classified
and their ancestors recomputed) and the tree is re-rendered once, rather
than triggering a full rescan — so the synced section reflects the new
state without the whole tree flashing (see `emeld--update-after-copy')."
  (interactive)
  (when emeld--git-mode
    (user-error "Copy is not available when comparing git revisions"))
  ;; Fail fast (before any prompt/copy) if rsync is required but missing.
  (when (and (eq emeld-copy-method 'rsync)
             (not (executable-find emeld-rsync-command)))
    (user-error "`emeld-copy-method' is `rsync' but %s is not on `exec-path'"
                emeld-rsync-command))
  (let* ((use-rsync (emeld--copy-use-rsync-p))
         (mirror (and use-rsync emeld-copy-mirror))
         (jobs (emeld--copy-jobs))
         (deletes (cl-remove-if-not (lambda (j) (eq (car-safe j) :delete)) jobs))
         (copies  (cl-remove-if     (lambda (j) (eq (car-safe j) :delete)) jobs))
         (nc (length copies)) (nd (length deletes))
         ;; Relative paths touched (copies + deletes), for the surgical update.
         (rels (delq nil (append
                          (mapcar (lambda (j) (emeld--rel-from-abs (car j))) copies)
                          (mapcar (lambda (j) (emeld--rel-from-abs (cdr j))) deletes))))
         (prompt (cond
                  ((zerop nd)
                   (format "%s %d node(s) to the other side%s%s? "
                           (if mirror "Mirror" "Copy") nc
                           (if use-rsync " via rsync" "")
                           (if mirror " (deletes destination-only files in dirs)" "")))
                  ((zerop nc)
                   (format "Delete %d node(s) from the other side to sync (to trash)? " nd))
                  (t
                   (format "Sync: copy %d and delete %d node(s) to the other side%s? "
                           nc nd (if use-rsync " (copies via rsync)" ""))))))
    (when (yes-or-no-p prompt)
      ;; Deletions first (synchronous, to trash); the surgical update then
      ;; drops those now-absent nodes from the tree.
      (dolist (j deletes)
        (let ((path (cdr j)))
          (when (file-exists-p path) (ignore-errors (emeld--delete-one path)))))
      (cond
       (copies (if use-rsync
                   (emeld--copy-async copies rels)
                 (emeld--copy-sync copies rels)))
       (t (clrhash emeld--marked-paths)
          (emeld--update-after-copy rels)
          (message "Deleted %d node(s) to sync" nd))))))

(defun emeld--delete-one (path)
  "Delete the file or directory at PATH recursively, moving it to trash."
  (if (file-directory-p path)
      (delete-directory path t t)
    (delete-file path t)))

(defun emeld-delete ()
  "Delete files/directories on their marked side(s).
When marks exist, each marked node is deleted from every side it is marked
on (a node marked on both sides is deleted from both).  Otherwise the node at
point is deleted from the current side (the column point is in)."
  (interactive)
  (when emeld--git-mode
    (user-error "Delete is not available when comparing git revisions"))
  (let* ((on-left (< (current-column) emeld--column-width))
         (side (if on-left "left" "right"))
         (marked (let (paths)
                   (maphash (lambda (k _) (push k paths)) emeld--marked-paths)
                   (nreverse paths))))
    (if marked
        ;; Count side-marks so the prompt reflects what will actually be deleted.
        (let ((targets 0))
          (dolist (rel marked) (setq targets (+ targets (length (emeld--marked-sides rel)))))
          (when (yes-or-no-p (format "Delete %d marked file(s) on their marked side(s)? "
                                     targets))
            (let ((count 0) (errors 0) (missing 0))
              (dolist (rel marked)
                (dolist (s (emeld--marked-sides rel))
                  (let ((path (expand-file-name rel (if (eq s 'left)
                                                        emeld--left-root
                                                      emeld--right-root))))
                    (if (file-exists-p path)
                        (condition-case nil
                            (progn (emeld--delete-one path) (cl-incf count))
                          (error (cl-incf errors)))
                      (cl-incf missing)))))
              ;; Update only the affected nodes in place (no full rescan/flash);
              ;; `emeld--update-after-copy' re-classifies each from on-disk state
              ;; and drops nodes now gone from both sides.
              (clrhash emeld--marked-paths)
              (emeld--update-after-copy marked)
              (message "Deleted %d file(s)%s%s" count
                       (if (> missing 0) (format ", %d already gone" missing) "")
                       (if (> errors 0) (format " (%d errors)" errors) "")))))
      (let* ((node (get-text-property (point) 'emeld-node))
             (path (and node (if on-left (emeld-node-left-path node)
                               (emeld-node-right-path node)))))
        (unless node (user-error "No node at point"))
        (unless path (user-error "No file on %s side to delete" side))
        (when (yes-or-no-p (format "Delete %s from %s side?" (emeld-node-name node) side))
          (condition-case err
              (progn
                (emeld--delete-one path)
                ;; Local in-place update of just this node (no full rescan/flash).
                (emeld--update-after-copy (list (emeld--node-relpath node)))
                (message "Deleted %s from %s side" (emeld-node-name node) side))
            (error (message "Delete failed: %s" (error-message-string err)))))))))

(defun emeld-swap-panes ()
  "Swap the left and right comparison directories and flip all node paths.
In git mode the two revisions are swapped and the comparison re-scanned."
  (interactive)
  (if emeld--git-mode
      (progn
        (cl-rotatef emeld--git-left-rev emeld--git-right-rev)
        (setf (emeld-node-name emeld--root-node)
              (emeld--git-rev-label emeld--git-left-rev)
              (emeld-node-name-right emeld--root-node)
              (emeld--git-rev-label emeld--git-right-rev))
        (emeld-refresh)
        (message "Swapped revisions: %s <-> %s"
                 (emeld--git-rev-label emeld--git-left-rev)
                 (emeld--git-rev-label emeld--git-right-rev)))
    (unless (and emeld--left-root emeld--right-root)
      (user-error "No directories loaded to swap"))
    ;; 1. Swap the root directory variables
    (cl-rotatef emeld--left-root emeld--right-root)
    ;; 2. Recursively swap paths and flip side-exclusive statuses in the tree
    (cl-labels ((swap-node (node)
                  (when node
                    ;; Swap left and right paths
                    (cl-rotatef (emeld-node-left-path node)
                                (emeld-node-right-path node))
                    ;; Flip side-exclusive diff statuses
                    (cl-case (emeld-node-diff-status node)
                      (left-only (setf (emeld-node-diff-status node) 'right-only))
                      (right-only (setf (emeld-node-diff-status node) 'left-only)))
                    ;; Recurse into children
                    (mapc #'swap-node (emeld-node-children node)))))
      ;; Swap the root node's display names too
      (let ((old-name (emeld-node-name emeld--root-node))
            (old-name-right (emeld-node-name-right emeld--root-node)))
        (setf (emeld-node-name emeld--root-node) (or old-name-right old-name))
        (setf (emeld-node-name-right emeld--root-node) old-name))
      (swap-node emeld--root-node))
    ;; 3. Redraw
    (emeld--render)
    (message "Swapped left and right panes.")))

(defun emeld--toggle-indicator (label enabled)
  "Return a string for toggle LABEL showing ENABLED state.
A leading ASCII checkbox prefixes LABEL: \"[x] \" when ENABLED, \"[ ] \"
when not.  Plain ASCII is used (rather than Unicode check glyphs) so the
indicator renders correctly on Windows consoles and in terminals without
Unicode support.  The checkbox alone signals state (dimming the off labels
can make them unreadable on some themes); LABEL's first letter is bold to
hint at the key that toggles it (e.g. \"S\" in \"Same\")."
  (concat (if enabled "[x] " "[ ] ")
          (if (> (length label) 0)
              (concat (propertize (substring label 0 1) 'face 'bold)
                      (substring label 1))
            label)))

(defun emeld--display-state-string ()
  "Return a string showing the primary Same/New/Modified filter states.
Each label is spelled out with a leading ASCII checkbox.  Scan-affecting
notes are appended after them: the whitespace mode (-w/-b) when active, a
`warning'-faced \"(!) gitignore off\" note while `emeld-respect-gitignore'
is off (git-ignored files are then included in the scan — the exception
to the default of dropping them, which needs no flag), and a
`warning'-faced \"(!) files hidden\" note when `emeld-show-files' is nil,
since a files-less tree can look empty and be mistaken for
\"no differences\".  The secondary display
toggles (Files/Tree-lines/Strikethrough/Gitignore) are intentionally
omitted here to keep the header line uncluttered; they remain reachable
from the transient (\\[emeld-dispatch])."
  (concat
   (emeld--toggle-indicator "Same" emeld-show-same)
   " "
   (emeld--toggle-indicator "New" emeld-show-added)
   " "
   (emeld--toggle-indicator "Modified" emeld-show-different)
   (when emeld-ignore-whitespace
     (if (eq emeld-ignore-whitespace 'all) " -w" " -b"))
   ;; Git-ignored files are dropped from the scan by default, so that
   ;; needs no flag; warn only when respect is off (the exception),
   ;; since ignored files are then included.
   (unless emeld-respect-gitignore
     (propertize " (!) gitignore off" 'face 'warning))
   (unless emeld-show-files
     (propertize " (!) files hidden" 'face 'warning))
   ;; Session-only temporary hides (`emeld-hide-at-point'); flag the count so
   ;; the user knows items are tucked away and how to bring them back.
   (when emeld--hidden-paths
     (propertize (format " (!) %d hidden (C-u C-k restores)"
                         (length emeld--hidden-paths))
                 'face 'warning))))

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
         (format " Scanning… %d entries" emeld--scan-diff-count)
       " Scanning…"))
    ('building-tree
     (cond
      ((zerop emeld--scan-total) " Building tree…")
      ((< emeld--scan-processed emeld--scan-total)
       (let ((frac (/ (float emeld--scan-processed) emeld--scan-total)))
         (format " Building tree %s %d%%"
                 (emeld--progress-bar frac) (round (* 100 frac)))))
      (t " Assembling nodes…")))
    ;; When the scan is finished or idle there is nothing in progress to
    ;; report, so show no status text at all — only the filter legend.
    (_ "")))

(defun emeld--update-header-line ()
  "Install the header-line format showing scan progress, status and legend.
The format is an `:eval' form, so it re-evaluates on each redisplay and
reflects the live scan counters without needing to be re-installed."
  (setq header-line-format
        '(:eval
          (concat (emeld--scan-progress-string)
                  " " (emeld--display-state-string) " "
                  ;; Flag a git revision comparison and which revs.
                  (when emeld--git-mode
                    (propertize (format "[git %s→%s] "
                                        (emeld--git-rev-label emeld--git-left-rev)
                                        (emeld--git-rev-label emeld--git-right-rev))
                                'face 'bold))
                  ;; History stepping position (`,'/`.').
                  (when (and emeld--git-mode emeld--git-history)
                    (propertize (if (< emeld--git-history-index 0)
                                    (format "[hist WT/%d] "
                                            (length emeld--git-history))
                                  (format "[hist %d/%d] "
                                          (1+ emeld--git-history-index)
                                          (length emeld--git-history)))
                                'face 'warning))
                  ;; Flag a depth-limited (shallow) scan.
                  (when emeld--scan-max-depth
                    (propertize (format "[depth %d] " emeld--scan-max-depth)
                                'face 'warning))
                  ;; Mirror copies can delete destination files, so flag it.
                  (when emeld-copy-mirror
                    (propertize "[Mirror] " 'face 'warning))
                  ;; Wide is the default, so flag only the more compact densities.
                  (unless (eq emeld-tree-density 'wide)
                    (propertize (format "[%s] "
                                        (capitalize (symbol-name emeld-tree-density)))
                                'face 'bold))))))

(defun emeld--current-node ()
  "Return the emeld-node text-property at point."
  (get-text-property (point) 'emeld-node))

(defvar emeld--render-seek-path nil
  "Relpath `emeld--render' wants point to land on, bound during a render.")
(defvar emeld--render-seek-is-dir nil
  "Whether `emeld--render-seek-path' names a directory node.")
(defvar emeld--render-seek-pos nil
  "Line start of the matched seek node, set by `emeld--render-nodes'.")

(defun emeld--render ()
  "Render the current visible tree into the comparison buffer, preserving point."
  (let* ((inhibit-read-only t)
         (inhibit-modification-hooks t)
         (current-node (emeld--current-node))
         (current-col (current-column))
         (target-path (and current-node (emeld--node-relpath current-node)))
         (target-is-dir (and current-node (emeld-node-is-dir current-node)))
         (fallback-line (line-number-at-pos)))
    
    (setq emeld--column-width (/ (* (window-width) emeld-column-ratio) 100))
    (buffer-disable-undo)
    (erase-buffer)
    (remove-overlays)
    
    (cond
     ;; Scan in progress, no nodes arrived yet
     ((and (memq emeld--status-phase '(running-diff building-tree))
           (or (null emeld--root-node)
               (null (emeld-node-children emeld--root-node))))
      (insert "\n")
      (insert (propertize "  Scanning directories, please wait..." 'face 'font-lock-comment-face))
      (insert "\n"))
     
     ;; Done or no-tree — always render headers and root structure for reference
     (t
      ;; Status banner when the tree is empty (no differences)
      (when (and (eq emeld--status-phase 'done)
                 (or (null emeld--root-node)
                     (null (emeld-node-children emeld--root-node))))
        (insert "\n")
        (insert (propertize (if emeld--git-mode
                                "  No differences between the two revisions."
                              "  No differences detected between directories.")
                            'face 'font-lock-comment-face))
        (insert "\n\n"))

      ;; Headers (with emeld-header property for interactive directory/revision switching)
      (let ((header-start (point))
            (left-label (if emeld--git-mode
                            (emeld--git-rev-label emeld--git-left-rev)
                          (abbreviate-file-name emeld--left-root)))
            (right-label (if emeld--git-mode
                             (emeld--git-rev-label emeld--git-right-rev)
                           (abbreviate-file-name emeld--right-root))))
        (insert (propertize (truncate-string-to-width left-label
                                                      emeld--column-width nil ?\s)
                            'face 'emeld-header-face))
        (insert (propertize (truncate-string-to-width right-label
                                                      emeld--column-width nil ?\s)
                            'face 'emeld-header-face))
        (add-text-properties header-start (point) '(emeld-header t mouse-face highlight)))
      (insert "\n")
      ;; Git mode: a foldable block of commit context below the revision
      ;; header.
      (when emeld--git-mode (emeld--insert-git-commit-block))
      ;; Root node (collapsible) — draws even with no children so the
      ;; directory structure stays visible for visual reference.  The seek
      ;; vars let the render itself note where the target node landed (O3).
      (let ((emeld--render-seek-path target-path)
            (emeld--render-seek-is-dir target-is-dir)
            (emeld--render-seek-pos nil))
        (when emeld--root-node
          (emeld--render-nodes (list emeld--root-node) "" emeld--column-width))
        ;; Restore point: jump to the node matched during render, else the
        ;; structural fallback line.
        (if (and target-path emeld--render-seek-pos)
            (progn (goto-char emeld--render-seek-pos)
                   (move-to-column current-col))
          (goto-char (point-min))
          (forward-line (1- (min fallback-line (count-lines (point-min) (point-max)))))
          ;; The line we landed on may be a header (e.g. the copied
          ;; node dropped out of the visible tree); snap to the nearest node
          ;; line so point always rests on a node, not a structural line.
          (unless (get-text-property (point) 'emeld-node)
            (let ((fwd (next-single-property-change (point) 'emeld-node)))
              (cond
               (fwd (goto-char fwd))
               ((setq fwd (previous-single-property-change (point) 'emeld-node))
                (goto-char (1- fwd))
                (beginning-of-line)))))
          ;; Align to the node name on the side point started in (not column 0
          ;; / the tree-line glyphs) so point sits on the node text.
          (when (get-text-property (point) 'emeld-node)
            (emeld--move-to-node-name (>= current-col emeld--column-width)))))))
    
    (set-buffer-modified-p nil)))

(defun emeld--node-visible-p (node)
  "Determine if NODE should be visible based on status and structure filters.
The root node is always visible.  When showing identical files, top-level
subdirectories are exposed for structural reference; with identical files
hidden, an all-identical (`same') directory is hidden too, so toggling Same
off returns to a pure diff view.  Directories containing changes are always
shown so collapsed folder rows remain navigable.  Nodes temporarily
hidden this session (`emeld--hidden-paths', via `emeld-hide-at-point') are
never visible regardless of the other rules."
  (and
   ;; Session-only temporary hide; the relpath walk is skipped entirely
   ;; unless something is actually hidden.
   (not (and emeld--hidden-paths
             (member (emeld--node-relpath node) emeld--hidden-paths)))
   (or (eq node emeld--root-node)
      ;; Expose top-level subdirectories for structural context, but only while
      ;; showing identical files; otherwise a `same' dir must not surface.
      (and emeld-show-same
           (emeld-node-is-dir node)
           (eq (emeld-node-parent node) emeld--root-node))
      (and (emeld-node-is-dir node)
           (not (eq (emeld-node-diff-status node) 'same)))
      (and emeld-show-files
           (let ((status (emeld-node-diff-status node)))
             (cond ((eq status 'same) emeld-show-same)
                   ((eq status 'different) emeld-show-different)
                   ((memq status '(left-only right-only)) emeld-show-added)
                   (t t)))))))

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
      (let ((ws (emeld--whitespace-args)))
        (cond
         ((eq status 'different)
          (apply #'call-process "diff" nil t nil "-u" (append ws (list right left))))
         ((eq status 'left-only)
          (apply #'call-process "diff" nil t nil "-u" (append ws (list "/dev/null" left))))
         ((eq status 'right-only)
          (apply #'call-process "diff" nil t nil "-u" (append ws (list right "/dev/null"))))))
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

(defun emeld--tree-branch-width ()
  "Column width of a node's branch connector under the current density."
  (if (eq emeld-tree-density 'extreme) 1 emeld-indent-offset))

(defun emeld--tree-branch (is-last)
  "Return the branch connector string for a node; IS-LAST marks the last sibling.
Honors `emeld-show-tree-lines' and `emeld-tree-density'."
  (if emeld-show-tree-lines
      (if (eq emeld-tree-density 'extreme)
          (if is-last "└" "├")
        (concat (if is-last "└" "├")
                (make-string (1- emeld-indent-offset) ?─)))
    (make-string (emeld--tree-branch-width) ?\s)))

(defun emeld--tree-continuation (is-last)
  "Return the child-prefix continuation string drawn under a node.
IS-LAST marks the last sibling (so its trunk is blank rather than `│').
Honors `emeld-show-tree-lines' and `emeld-tree-density'."
  (if emeld-show-tree-lines
      (pcase emeld-tree-density
        ('extreme (if is-last " " "│"))
        ('wide (concat (if is-last " " "│")
                       (make-string emeld-indent-offset ?\s)))
        (_ (concat (if is-last " " "│")
                   (make-string (1- emeld-indent-offset) ?\s))))
    ;; Tree-lines off: mirror the per-level widths above so `wide' still
    ;; indents one column more than `compact' (it would otherwise collapse
    ;; to the same width as the branch).
    (make-string (pcase emeld-tree-density
                   ('extreme 1)
                   ('wide (1+ emeld-indent-offset))
                   (_ emeld-indent-offset))
                 ?\s)))

(defun emeld--render-nodes (nodes indent-prefix column-width &optional rel-prefix)
  "Render a list of NODES with INDENT-PREFIX and COLUMN-WIDTH.
REL-PATH is threaded down as REL-PREFIX (the parent's relative path) so each
node's relpath is built incrementally rather than walked up to the root per
node — see `emeld--node-relpath' for the equivalent slow form."
  (let ((count (length nodes))
        (idx 0))
    (dolist (node nodes)
      (setq idx (1+ idx))
      (when (emeld--node-visible-p node)
        (let* ((status (emeld-node-diff-status node))
               (is-last (= idx count))
               (branch (emeld--tree-branch is-last))
               (full-prefix (concat indent-prefix branch))
               (face (cond ((eq status 'same) 'emeld-same-face)
                           ((eq status 'different) 'emeld-diff-face)
                           ((eq status 'left-only) 'emeld-left-only-face)
                           ((eq status 'right-only) 'emeld-right-only-face)))
               (name-face (if (emeld-node-is-dir node) 'emeld-dir-face face))
               (dir-indicator (if (emeld-node-is-dir node)
                                  (concat (if (emeld-node-expanded node)
                                              emeld-fold-expanded-indicator
                                            emeld-fold-collapsed-indicator)
                                          emeld-dir-prefix)
                                ""))
               ;; Mark directories a depth-limited scan left unscanned.
               (scan-marker (if (and (emeld-node-is-dir node)
                                     (not (emeld-node-scanned node)))
                                (propertize " …" 'face 'shadow)
                              "")))
          (let* ((start (point))
                 ;; Build relpath from the parent's (O2) instead of walking to
                 ;; the root for every node on every render.
                 (rel (if (emeld-node-parent node)
                          (if (string-empty-p (or rel-prefix ""))
                              (emeld-node-name node)
                            (concat rel-prefix "/" (emeld-node-name node)))
                        ""))
                 (lmarked (emeld--marked-side-p rel 'left))
                 (rmarked (emeld--marked-side-p rel 'right))
                 ;; Tag each mark cell with `emeld-mark-cell' so a single
                 ;; mark/unmark can rewrite just that glyph (`emeld--update-line-mark')
                 ;; instead of re-rendering the whole tree.
                 (left-mark (propertize (if lmarked "*" " ")
                                        'face (and lmarked 'font-lock-keyword-face)
                                        'emeld-mark-cell 'left))
                 (right-mark (propertize (if rmarked "*" " ")
                                         'face (and rmarked 'font-lock-keyword-face)
                                         'emeld-mark-cell 'right)))
            ;; Record this node's line start when it is the point-restore target
            ;; (O3), so the caller jumps straight here after the render rather
            ;; than rescanning every line.
            (when (and emeld--render-seek-path
                       (null emeld--render-seek-pos)
                       (eq (emeld-node-is-dir node) emeld--render-seek-is-dir)
                       (string= rel emeld--render-seek-path))
              (setq emeld--render-seek-pos start))
            ;; Left mark column
            (insert left-mark " ")
            ;; Left Side
            (insert (propertize full-prefix 'face 'shadow))
            (if (emeld-node-left-path node)
                (progn
                  (insert dir-indicator)
                  (insert (propertize (emeld-node-name node) 'face name-face))
                  (insert scan-marker))
              (when (and emeld-show-strikethrough (eq status 'right-only))
                (insert dir-indicator)
                (insert (propertize (emeld-node-name node)
                                    'face 'emeld-right-only-strike-face))))

            ;; Padding to Right Side
            (let ((current-len (current-column)))
              (if (< current-len column-width)
                  (insert (make-string (- column-width current-len) ?\s))
                (insert "  ")))

            ;; Right mark column + Right Side
            (insert right-mark " ")
            (insert (propertize full-prefix 'face 'shadow))
            (if (emeld-node-right-path node)
                (progn
                  (insert dir-indicator)
                  (insert (propertize (or (emeld-node-name-right node)
                                          (emeld-node-name node))
                                      'face name-face))
                  (insert scan-marker))
              (when (and emeld-show-strikethrough (eq status 'left-only))
                (insert dir-indicator)
                (insert (propertize (emeld-node-name node)
                                    'face 'emeld-left-only-strike-face))))
            
            (add-text-properties start (point) `(emeld-node ,node mouse-face highlight))
            (insert "\n")

            ;; Inline diff preview (TAB on a file): a Magit-style quick peek
            ;; rendered full-width beneath the row, indented under the name.
            ;; Lines carry the same `emeld-node' so TAB/SPC/= and navigation
            ;; act on the file, and TAB again collapses the preview.
            (when (and (not (emeld-node-is-dir node)) (emeld-node-inline-diff node))
              ;; The two leading spaces stand in for the mark column that
              ;; precedes `indent-prefix' on every tree row, so the tree guides
              ;; in `indent-prefix' line up vertically with the rows above; the
              ;; remaining offset spaces indent the diff text under the name.
              (let ((diff-prefix (concat "  " indent-prefix
                                         (make-string (emeld--tree-branch-width) ?\s))))
                (dolist (dl (emeld-node-inline-diff node))
                  (let ((dstart (point)))
                    (insert diff-prefix dl "\n")
                    (add-text-properties dstart (point)
                                         `(emeld-node ,node emeld-inline-diff t))))))

            ;; Recurse.  The continuation indent depends on `emeld-tree-density':
            ;; `compact' matches the branch width for a tight tree, `wide' adds
            ;; one space so a child's connector hangs past the parent's content,
            ;; and `extreme' collapses each level to a single column.  Either
            ;; way files and dirs use the same indent, so siblings always align
            ;; under their parent (see `emeld--tree-continuation').
            (when (and (emeld-node-expanded node) (emeld-node-children node))
              (emeld--render-nodes (emeld-node-children node)
                                   (concat indent-prefix
                                           (emeld--tree-continuation is-last))
                                   column-width rel))))))))

(defclass emeld-dynamic-suffix (transient-suffix)
  ((dynamic-description :initarg :dynamic-description :initform nil))
  "A transient suffix that evaluates a lambda for its description.")

(cl-defmethod transient-format-description ((obj emeld-dynamic-suffix))
  "Evaluate the dynamic-description slot for the display string."
  (let ((fn (oref obj dynamic-description)))
    (if fn (funcall fn) (cl-call-next-method))))

(transient-define-prefix emeld-dispatch-view ()
  "Emeld view, visibility and layout options."
  [["Show"
    ("F" (lambda () (if emeld-show-files "[x] Files" "[ ] Files"))
     (lambda () (interactive) (emeld-toggle-show-files)) :transient t)
    ("T" (lambda () (if emeld-show-tree-lines "[x] tree-lines" "[ ] tree-lines"))
     (lambda () (interactive) (emeld-toggle-tree-lines)) :transient t)
    ("z" (lambda () (format "density: %s" emeld-tree-density))
     (lambda () (interactive) (emeld-cycle-tree-density)) :transient t)
    ("s" (lambda () (if emeld-show-strikethrough "[x] strikethrough" "[ ] strikethrough"))
     (lambda () (interactive) (emeld-toggle-show-strikethrough)) :transient t)
    ("I" (lambda () (if emeld-respect-gitignore "[x] gitignore" "[ ] gitignore"))
     (lambda () (interactive) (emeld-toggle-gitignore)))
    ("i" (lambda ()
           (pcase emeld-ignore-whitespace
             ('change "[-b] ignore ws (amount)")
             ('all "[-w] ignore ws (all)")
             (_ "[ ] ignore ws")))
     (lambda () (interactive) (emeld-toggle-ignore-whitespace)))
    ("c" (lambda () (if emeld--git-commit-expanded "[x] commit msg" "[ ] commit msg"))
     (lambda () (interactive) (emeld-git-toggle-commit-message)) :transient t
     :if (lambda () emeld--git-mode))]
   ["Layout"
    ("[" "collapse all"  emeld-collapse-all :transient t)
    ("]" "expand all"    emeld-expand-all :transient t)
    ("/" "expand to changes" emeld-expand-changes :transient t)
    ("<backtab>" "cycle visibility" emeld-cycle-visibility :transient t)
    (">" "indent more"  emeld-increase-indent :transient t)
    ("<" "indent less"  emeld-decrease-indent :transient t)
    ("}" "pane wider"   emeld-grow-column :transient t)
    ("{" "pane narrow"  emeld-shrink-column :transient t)]
   ["Hide (session)"
    ("C-k" "hide node (C-u: unhide all)" emeld-hide-at-point)
    ("U"   "unhide all"                  emeld-unhide-all)]])

(transient-define-prefix emeld-dispatch-file ()
  "Emeld file actions."
  [["Open"
    ("RET" "action (smart)"     emeld-action)
    ("e"   "ediff"              emeld-ediff)
    ("="   "diff / open / fold" emeld-soft-diff)
    ("SPC" "diff / open / fold" emeld-soft-diff)
    ("TAB" "peek inline diff (file)" emeld-toggle-inline-diff :transient t)]
   ["Dired"
    ("A"   "dired left side"    emeld-dired-left)
    ("B"   "dired right side"   emeld-dired-right)
    ("w"   "copy path"          emeld-copy-path-at-point)]
   ["Diff"
    ("D"   "diff root (recursive)" emeld-diff-root)
    ("d"   "diff dir at point"   emeld-diff-dir)
    ("P"   "format as patch"    emeld-format-patch)]
   ["Sync"
    ("C"   "sync to other side (copy/delete)" emeld-copy)
    ("R" (lambda () (if emeld-copy-mirror "[x] mirror copy (--delete)" "[ ] mirror copy (--delete)"))
     (lambda () (interactive) (emeld-toggle-mirror)) :transient t)
    ("x"   "delete marked/at point" emeld-delete)]])

(transient-define-prefix emeld-dispatch-mark ()
  "Emeld marking."
  ["Mark"
   ("m"   "mark"            emeld-mark :transient t)
   ("u"   "unmark"          emeld-unmark :transient t)
   ("* m" "modified"        emeld-mark-modified :transient t)
   ("* l" "left-only (new)" emeld-mark-left-only :transient t)
   ("* c" "→ mirror right"  emeld-mark-to-copy :transient t)
   ("* a" "all changed (mirror)" emeld-mark-all :transient t)
   ("U"   "unmark all"      emeld-unmark-all :transient t)])

(transient-define-prefix emeld-dispatch-filter ()
  "Emeld filename filter groups."
  ["Filter"
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
                    'emeld-dispatch-filter
                    (list key "" cmd
                          :class 'emeld-dynamic-suffix
                          :dynamic-description desc-func
                          :transient t))
                   suffixes)))
         (nreverse suffixes))))])

(transient-define-prefix emeld-dispatch-dir-locals ()
  "Emeld `.dir-locals.el' commands."
  ["Dir-locals"
   ("e" "edit .dir-locals.el…" emeld-edit-dir-locals)
   ("&" "filter node at point → .dir-locals…" emeld-add-filter-at-point)])

(transient-define-prefix emeld-dispatch ()
  "Transient menu for emeld."
  [["Filename"
    ("F" "Filter →"     emeld-dispatch-filter)
    ("L" "Dir-locals →" emeld-dispatch-dir-locals)]
   ["Preset"
    ("s" "save current" emeld-save-preset)
    ("l" "load…"        emeld-load-preset)
    ("d" "delete…"      emeld-delete-preset)]
   ["Version Control"
    ("G" "git revisions…"    emeld-git-diff)
    ("H" "git history step…" emeld-git-step-history)]
   ["Menus"
    ("v" "View →"     emeld-dispatch-view)
    ("f" "File →"     emeld-dispatch-file)
    ("m" "Mark →"     emeld-dispatch-mark)]
   ])

(define-key emeld-mode-map (kbd "?") #'emeld-dispatch)

(provide 'emeld-core)
;;; emeld-core.el ends here
