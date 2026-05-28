;;; emeld-tests.el --- Tests for emeld.el -*- lexical-binding: t; -*-

(load-file "emeld.el")
(require 'ert)

(ert-deftest emeld-glob-matching ()
  "Test glob to regexp conversion and matching."
  (let ((emeld-filter-groups '(("Test" "*.elc" "tmp/*" ".git")))
        (emeld--active-filter-groups '("Test")))
    (should (emeld--filtered-p "test.elc" "src/test.elc"))
    (should-not (emeld--filtered-p "test.el" "src/test.el"))
    (should (emeld--filtered-p "foo" "tmp/foo"))
    (should (emeld--filtered-p ".git" ".git"))))

(ert-deftest emeld-group-active-default ()
  "Test default active state from filter groups."
  (should (emeld--group-active-default '("Active" t "*.elc")))
  (should-not (emeld--group-active-default '("Inactive" "*.elc")))
  (should-not (emeld--group-active-default '("Explicit inactive" nil "*.elc")))
  ;; Should be nil for a group with no boolean
  (should-not (emeld--group-active-default '("No bool" "*.pyc" "*.pyc"))))


(ert-deftest emeld-group-globs ()
  "Test extracting globs from filter groups."
  (should (equal (emeld--group-globs '("Active" t "*.elc" "*.pyc"))
                 '("*.elc" "*.pyc")))
  (should (equal (emeld--group-globs '("Inactive" "*.elc" "*.pyc"))
                 '("*.elc" "*.pyc")))
  (should (equal (emeld--group-globs '("Explicit" nil "*.o"))
                 '("*.o"))))

(ert-deftest emeld-read-gitignore-simple-patterns ()
  "Test extracting simple glob patterns from .gitignore.
Patterns with leading/trailing slashes are stripped; those with
internal slashes are skipped."
  (let* ((dir (make-temp-file "emeld-test" t)))
    (unwind-protect
        (let ((gitignore (expand-file-name ".gitignore" dir)))
          (with-temp-file gitignore
            (insert (string-join
                     '("# comment"
                       "*.o"
                       "build/"
                       "*.log"
                       "/src/generated"
                       "!keep.el"
                       ""
                       "*.pyc"
                       "/.idea"
                       "/.agent-shell/")
                     "\n")))
          (should (equal (emeld--read-gitignore-simple-patterns dir)
                         '("*.o" "build" "*.log" "*.pyc" ".idea" ".agent-shell"))))
      (delete-directory dir t))))

(ert-deftest emeld-git-ignored-p ()
  "Test git-ignore detection with a temporary git repository."
  (skip-unless (executable-find "git"))
  (let* ((dir (make-temp-file "emeld-test" t))
         (emeld-respect-gitignore t)
         (emeld--gitignore-cache nil))
    (unwind-protect
        (let ((gitignore (expand-file-name ".gitignore" dir))
              (log-file (expand-file-name "test.log" dir))
              (el-file (expand-file-name "test.el" dir)))
          (call-process "git" nil nil nil "init" "-q" dir)
          (call-process "git" nil nil nil "-C" dir "config" "user.email" "test@test.com")
          (call-process "git" nil nil nil "-C" dir "config" "user.name" "Test")
          (with-temp-file gitignore
            (insert "*.log\n"))
          (call-process "git" nil nil nil "-C" dir "add" ".gitignore")
          (call-process "git" nil nil nil "-C" dir "commit" "-q" "-m" "init")
          (with-temp-file log-file (insert "log"))
          (with-temp-file el-file (insert "el"))
          (should (emeld--git-ignored-p "test.log" dir))
          (should-not (emeld--git-ignored-p "test.el" dir))
          ;; Disabling the feature short-circuits regardless of git state.
          (let ((emeld-respect-gitignore nil))
            (should-not (emeld--git-ignored-p "test.log" dir))))
      (delete-directory dir t))))

(ert-deftest emeld-git-ignored-p-nested ()
  "Files inside a wholly-ignored directory are detected via ancestor walk.
`git ls-files --directory' collapses such directories, so membership has
to climb parents rather than match the leaf path directly."
  (skip-unless (executable-find "git"))
  (let* ((dir (make-temp-file "emeld-test" t))
         (emeld-respect-gitignore t)
         (emeld--gitignore-cache nil))
    (unwind-protect
        (progn
          (call-process "git" nil nil nil "init" "-q" dir)
          (call-process "git" nil nil nil "-C" dir "config" "user.email" "test@test.com")
          (call-process "git" nil nil nil "-C" dir "config" "user.name" "Test")
          (with-temp-file (expand-file-name ".gitignore" dir)
            (insert "build/\n*.log\n"))
          (call-process "git" nil nil nil "-C" dir "add" ".gitignore")
          (call-process "git" nil nil nil "-C" dir "commit" "-q" "-m" "init")
          (make-directory (expand-file-name "src/build" dir) t)
          (with-temp-file (expand-file-name "src/build/out.o" dir) (insert "o"))
          (with-temp-file (expand-file-name "src/app.log" dir) (insert "l"))
          (with-temp-file (expand-file-name "src/app.el" dir) (insert "e"))
          ;; The collapsed directory itself and a file beneath it.
          (should (emeld--git-ignored-p "src/build" dir))
          (should (emeld--git-ignored-p "src/build/out.o" dir))
          ;; A nested pattern match and a kept sibling.
          (should (emeld--git-ignored-p "src/app.log" dir))
          (should-not (emeld--git-ignored-p "src/app.el" dir)))
      (delete-directory dir t))))

(ert-deftest emeld-git-ignored-p-absolute-terminates ()
  "An absolute REL-PATH must not spin forever in the ancestor walk.
Regression for a freeze where the walk fixpointed at \"/\".  The test
runs under a timeout so an infinite loop fails rather than hangs."
  (let* ((emeld-respect-gitignore t)
         (emeld--gitignore-cache (make-hash-table :test #'equal))
         (set (make-hash-table :test #'equal)))
    (puthash "drivers" t set)
    (puthash (file-name-as-directory (expand-file-name "/abs/root")) set
             emeld--gitignore-cache)
    (with-timeout (5 (ert-fail "emeld--git-ignored-p did not terminate"))
      ;; Absolute paths cannot match the relative-keyed set, but they must
      ;; return (nil) rather than loop.
      (should-not (emeld--git-ignored-p "/home/x/foo" "/abs/root"))
      (should-not (emeld--git-ignored-p "/abs/root/drivers/gpu/x.c" "/abs/root"))
      (should-not (emeld--git-ignored-p "/" "/abs/root"))
      ;; Relative lookups against the same set still resolve correctly.
      (should (emeld--git-ignored-p "drivers/gpu/x.c" "/abs/root"))
      (should-not (emeld--git-ignored-p "kernel/sched.c" "/abs/root")))))

(ert-deftest emeld-git-ignored-set-pending-does-not-block ()
  "A `pending' cache entry returns nil without a synchronous compute.
This is what keeps the async pre-warm from ever blocking the UI."
  (let ((emeld--gitignore-cache (make-hash-table :test #'equal))
        (key (file-name-as-directory (expand-file-name "/some/root"))))
    (puthash key 'pending emeld--gitignore-cache)
    (should-not (emeld--git-ignored-set "/some/root"))
    ;; The entry stays `pending (no compute was attempted/cached over it).
    (should (eq 'pending (gethash key emeld--gitignore-cache)))))

(ert-deftest emeld-same-merge-no-duplicate-dirs ()
  "Adding identical files merges into existing dirs without duplication.
Guards the O(1) dir-merge optimisation: directories created by the
initial scan are pre-seeded into the path hash, so the same-file pass
must reuse them rather than create siblings with the same name."
  (let* ((emeld-respect-gitignore nil)
         (emeld-filter-groups nil)
         (emeld--active-filter-groups nil)
         (emeld--left-root "/l/")
         (emeld--right-root "/r/")
         (root (emeld-node-create :name "root" :is-dir t :expanded t))
         ;; Pre-existing dir "src" with one changed file, as the initial scan
         ;; would leave it.
         (src (emeld-node-create :name "src" :is-dir t :parent root
                                 :diff-status 'different))
         (path-hash (make-hash-table :test #'equal)))
    (setf (emeld-node-children src)
          (list (emeld-node-create :name "changed.c" :parent src
                                   :diff-status 'different)))
    (setf (emeld-node-children root) (list src))
    (emeld--seed-dir-path-hash root "" path-hash)
    ;; Two identical files in the existing dir, plus a new nested dir.
    (emeld--add-same-entry-to-tree root "src/a.c" path-hash "/l" "/r")
    (emeld--add-same-entry-to-tree root "src/b.c" path-hash "/l" "/r")
    (emeld--add-same-entry-to-tree root "src/gen/x.c" path-hash "/l" "/r")
    ;; Re-adding an identical file is deduped.
    (emeld--add-same-entry-to-tree root "src/a.c" path-hash "/l" "/r")
    (let* ((src-children (emeld-node-children src))
           (src-dirs (cl-remove-if-not #'emeld-node-is-dir src-children)))
      ;; Exactly one "src" under root (no duplicate dir node).
      (should (= 1 (length (cl-remove-if-not
                            (lambda (n) (string= (emeld-node-name n) "src"))
                            (emeld-node-children root)))))
      ;; changed.c + a.c + b.c + the gen/ dir = 4 children, a.c not doubled.
      (should (= 4 (length src-children)))
      (should (= 1 (length (cl-remove-if-not
                            (lambda (n) (string= (emeld-node-name n) "a.c"))
                            src-children))))
      ;; The nested dir was created once and holds x.c.
      (should (= 1 (length src-dirs)))
      (should (string= "gen" (emeld-node-name (car src-dirs))))
      (should (= 1 (length (emeld-node-children (car src-dirs))))))))

(ert-deftest emeld-filtered-p-with-gitignore ()
  "Test that emeld--filtered-p checks .gitignore when enabled."
  (skip-unless (executable-find "git"))
  (let* ((dir (make-temp-file "emeld-test" t)))
    (unwind-protect
        (let ((gitignore (expand-file-name ".gitignore" dir))
              (log-file (expand-file-name "test.log" dir))
              (el-file (expand-file-name "test.el" dir))
              (emeld-respect-gitignore t)
              (emeld-filter-groups nil)
              (emeld--active-filter-groups nil)
              (emeld--gitignore-cache nil)
              (emeld--left-root dir)
              (emeld--right-root nil))
          (call-process "git" nil nil nil "init" "-q" dir)
          (call-process "git" nil nil nil "-C" dir "config" "user.email" "test@test.com")
          (call-process "git" nil nil nil "-C" dir "config" "user.name" "Test")
          (with-temp-file gitignore
            (insert "*.log\n"))
          (call-process "git" nil nil nil "-C" dir "add" ".gitignore")
          (call-process "git" nil nil nil "-C" dir "commit" "-q" "-m" "init")
          (with-temp-file log-file (insert "log"))
          (with-temp-file el-file (insert "el"))
          (should (emeld--filtered-p "test.log" "test.log"))
          (should-not (emeld--filtered-p "test.el" "test.el")))
      (delete-directory dir t))))

(ert-deftest emeld-diff-exclude-args-with-gitignore ()
  "Test that --exclude args include gitignore patterns when enabled."
  (skip-unless (executable-find "git"))
  (let* ((dir (make-temp-file "emeld-test" t)))
    (unwind-protect
        (let ((gitignore (expand-file-name ".gitignore" dir))
              (emeld-respect-gitignore t)
              (emeld-filter-groups nil)
              (emeld--active-filter-groups nil)
              (emeld--left-root dir)
              (emeld--right-root nil))
          (call-process "git" nil nil nil "init" "-q" dir)
          (call-process "git" nil nil nil "-C" dir "config" "user.email" "test@test.com")
          (call-process "git" nil nil nil "-C" dir "config" "user.name" "Test")
          (with-temp-file gitignore
            (insert (string-join
                     '("# comment"
                       "*.o"
                       "build/"
                       "*.log"
                       "!keep.el"
                       ""
                       "*.pyc"
                       "/.idea"
                       "/.agent-shell/")
                     "\n")))
          (call-process "git" nil nil nil "-C" dir "add" ".gitignore")
          (call-process "git" nil nil nil "-C" dir "commit" "-q" "-m" "init")
          (let ((args (emeld--diff-exclude-args)))
            (should (member "--exclude=*.o" args))
            (should (member "--exclude=build" args))
            (should (member "--exclude=*.log" args))
            (should (member "--exclude=*.pyc" args))
            (should (member "--exclude=.idea" args))
            (should (member "--exclude=.agent-shell" args))
            (should-not (member "--exclude=!keep.el" args))
            (should (member "--exclude=node_modules" args))))
      (delete-directory dir t))))

(ert-deftest emeld-preset-save-load-delete ()
  "Presets save, overwrite in place, load to a comparison, and delete."
  (let ((emeld-presets nil)
        (emeld--left-root "/a/")
        (emeld--right-root "/b/"))
    (emeld-save-preset "p1")
    (should (equal (assoc "p1" emeld-presets) '("p1" "/a/" "/b/")))
    ;; Reusing a name overwrites it (new dirs), without duplicating.
    (let ((emeld--left-root "/x/") (emeld--right-root "/y/"))
      (emeld-save-preset "p1"))
    (should (= 1 (length emeld-presets)))
    (should (equal (assoc "p1" emeld-presets) '("p1" "/x/" "/y/")))
    ;; Loading dispatches to `emeld-diff' with the stored dirs.
    (let (captured)
      (cl-letf (((symbol-function 'emeld-diff)
                 (lambda (l r) (setq captured (list l r)))))
        (emeld-load-preset "p1"))
      (should (equal captured '("/x/" "/y/"))))
    ;; Deleting removes it.
    (emeld-delete-preset "p1")
    (should (null emeld-presets))))

(ert-deftest emeld-mark-by-status ()
  "Marking by status targets the right leaves for modified / left-only / sync."
  (let* ((root (emeld-node-create :name "root" :is-dir t))
         (dirA (emeld-node-create :name "dirA" :is-dir t :parent root
                                  :diff-status 'different))
         (f1 (emeld-node-create :name "f1" :parent dirA :diff-status 'different))
         (f2 (emeld-node-create :name "f2" :parent dirA :diff-status 'same))
         (f3 (emeld-node-create :name "f3" :parent root :diff-status 'left-only))
         (f4 (emeld-node-create :name "f4" :parent root :diff-status 'different))
         (f5 (emeld-node-create :name "f5" :parent root :diff-status 'right-only))
         (emeld--root-node root))
    (setf (emeld-node-children dirA) (list f1 f2))
    (setf (emeld-node-children root) (list dirA f3 f4 f5))
    ;; modified only — marked on the left (source) side by default
    (let ((emeld--marked-paths (make-hash-table :test #'equal)))
      (should (= 2 (emeld--mark-nodes-by-status '(different))))
      (should (emeld--marked-side-p "dirA/f1" 'left))
      (should (emeld--marked-side-p "f4" 'left))
      (should-not (emeld--marked-side-p "dirA/f1" 'right))
      (should-not (gethash "dirA/f2" emeld--marked-paths))
      (should-not (gethash "f3" emeld--marked-paths))
      (should-not (gethash "dirA" emeld--marked-paths)))
    ;; left-only only
    (let ((emeld--marked-paths (make-hash-table :test #'equal)))
      (should (= 1 (emeld--mark-nodes-by-status '(left-only))))
      (should (emeld--marked-side-p "f3" 'left)))
    ;; sync set (to copy left→right): modified + left-only, never right-only
    (let ((emeld--marked-paths (make-hash-table :test #'equal)))
      (should (= 3 (emeld--mark-nodes-by-status '(different left-only))))
      (should (emeld--marked-side-p "dirA/f1" 'left))
      (should (emeld--marked-side-p "f4" 'left))
      (should (emeld--marked-side-p "f3" 'left))
      (should-not (gethash "f5" emeld--marked-paths)))))

(ert-deftest emeld-mark-side-toggle ()
  "`emeld--set-mark' tracks per-side marks and drops empty entries."
  (let ((emeld--marked-paths (make-hash-table :test #'equal)))
    (emeld--set-mark "x" 'left t)
    (should (emeld--marked-side-p "x" 'left))
    (should-not (emeld--marked-side-p "x" 'right))
    (emeld--set-mark "x" 'right t)
    (should (emeld--marked-side-p "x" 'left))
    (should (emeld--marked-side-p "x" 'right))
    ;; Unmarking one side leaves the other and keeps the entry.
    (emeld--set-mark "x" 'left nil)
    (should-not (emeld--marked-side-p "x" 'left))
    (should (emeld--marked-side-p "x" 'right))
    (should (gethash "x" emeld--marked-paths))
    ;; Unmarking the last side removes the entry entirely.
    (emeld--set-mark "x" 'right nil)
    (should-not (gethash "x" emeld--marked-paths))))

(ert-deftest emeld-same-list-exclusions ()
  "The `list' same-merge keeps only genuinely-identical left files.
Existing file nodes and files under a left-only directory are excluded;
a new path that is neither is treated as identical."
  (let* ((root (emeld-node-create :name "root" :is-dir t))
         (src (emeld-node-create :name "src" :is-dir t :parent root
                                 :diff-status 'different))
         (d1 (emeld-node-create :name "d1.c" :parent src :diff-status 'different))
         (lo1 (emeld-node-create :name "new.c" :parent src :diff-status 'left-only))
         (lodir (emeld-node-create :name "newdir" :is-dir t :parent root
                                   :diff-status 'left-only))
         (skip (make-hash-table :test #'equal))
         (lod (make-hash-table :test #'equal)))
    (setf (emeld-node-children src) (list d1 lo1))
    (setf (emeld-node-children root) (list src lodir))
    (emeld--collect-same-exclusions root "" skip lod)
    (should (gethash "src/d1.c" skip))
    (should (gethash "src/new.c" skip))
    (should (gethash "newdir" lod))
    (should-not (gethash "src" skip))      ; non-left-only dir, recursed not skipped
    ;; a file under the left-only dir is excluded via ancestor walk
    (should (emeld--ancestor-in-set-p "newdir/deep/x.c" lod))
    (should-not (emeld--ancestor-in-set-p "src/fresh.c" lod))
    ;; the identical candidate is the one neither skipped nor under a left-only dir
    (should-not (gethash "src/fresh.c" skip))))

(ert-deftest emeld-ancestor-in-set-terminates ()
  "`emeld--ancestor-in-set-p' terminates on absolute / root paths."
  (let ((set (make-hash-table :test #'equal)))
    (puthash "a/b" t set)
    (with-timeout (5 (ert-fail "ancestor walk did not terminate"))
      (should (emeld--ancestor-in-set-p "a/b/c/d" set))
      (should-not (emeld--ancestor-in-set-p "/abs/x" set))
      (should-not (emeld--ancestor-in-set-p "/" set)))))

(ert-deftest emeld-diff-subdir-entries ()
  "`emeld--diff-subdir-entries' returns subdir-relative diff entries."
  (let* ((left (make-temp-file "emeld-l" t))
         (right (make-temp-file "emeld-r" t)))
    (unwind-protect
        (progn
          (dolist (pair '(("same.txt" "x" "x") ("diff.txt" "a" "b")))
            (with-temp-file (expand-file-name (nth 0 pair) left) (insert (nth 1 pair)))
            (with-temp-file (expand-file-name (nth 0 pair) right) (insert (nth 2 pair))))
          (with-temp-file (expand-file-name "onlyleft.txt" left) (insert "l"))
          (with-temp-file (expand-file-name "onlyright.txt" right) (insert "r"))
          (make-directory (expand-file-name "sub" left))
          (make-directory (expand-file-name "sub" right))
          (with-temp-file (expand-file-name "sub/deep.txt" left) (insert "1"))
          (with-temp-file (expand-file-name "sub/deep.txt" right) (insert "2"))
          (with-temp-buffer
            (setq-local emeld--left-root (file-name-as-directory left)
                        emeld--right-root (file-name-as-directory right)
                        emeld-respect-gitignore nil
                        emeld--active-filter-groups nil
                        emeld-show-same nil)
            (let ((entries (emeld--diff-subdir-entries left right)))
              (should (eq 'different (cdr (assoc "diff.txt" entries))))
              (should (eq 'different (cdr (assoc "sub/deep.txt" entries))))
              (should (eq 'left-only (cdr (assoc "onlyleft.txt" entries))))
              (should (eq 'right-only (cdr (assoc "onlyright.txt" entries))))
              (should-not (assoc "same.txt" entries)))
            ;; With show-same on, identical files are included.
            (setq-local emeld-show-same t)
            (let ((entries (emeld--diff-subdir-entries left right)))
              (should (eq 'same (cdr (assoc "same.txt" entries)))))))
      (delete-directory left t)
      (delete-directory right t))))

(ert-deftest emeld-refresh-subdir-rebuilds-subtree ()
  "`emeld-refresh-subdir' re-diffs the dir at point and splices in the result."
  (let* ((left (make-temp-file "emeld-l" t))
         (right (make-temp-file "emeld-r" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "d" left))
          (make-directory (expand-file-name "d" right))
          ;; Initially d/ holds one differing file.
          (with-temp-file (expand-file-name "d/a.txt" left) (insert "a"))
          (with-temp-file (expand-file-name "d/a.txt" right) (insert "b"))
          (with-temp-buffer
            (setq-local emeld--left-root (file-name-as-directory left)
                        emeld--right-root (file-name-as-directory right)
                        emeld-respect-gitignore nil
                        emeld--active-filter-groups nil
                        emeld-show-same nil
                        emeld--column-width 30)
            ;; Build a minimal tree: root → d (different) → a.txt (different).
            (let* ((root (emeld-node-create :name "root" :is-dir t))
                   (d (emeld-node-create :name "d" :is-dir t :parent root
                                         :diff-status 'different
                                         :left-path (expand-file-name "d" left)
                                         :right-path (expand-file-name "d" right))))
              (setf (emeld-node-children d)
                    (list (emeld-node-create :name "a.txt" :parent d :diff-status 'different)))
              (setf (emeld-node-children root) (list d))
              (setq-local emeld--root-node root)
              ;; On disk: a.txt now matches, and a new differing b.txt appears.
              (with-temp-file (expand-file-name "d/a.txt" right) (insert "a"))
              (with-temp-file (expand-file-name "d/b.txt" left) (insert "x"))
              (with-temp-file (expand-file-name "d/b.txt" right) (insert "y"))
              ;; Put point on a line tagged with the d node, and no-op the render.
              (insert (propertize "d" 'emeld-node d))
              (goto-char (point-min))
              (cl-letf (((symbol-function 'emeld--render) (lambda () nil)))
                (emeld-refresh-subdir))
              ;; a.txt is now identical (gone, since show-same is off); b.txt differs.
              (let ((names (mapcar #'emeld-node-name (emeld-node-children d))))
                (should (member "b.txt" names))
                (should-not (member "a.txt" names))
                (should (eq 'different (emeld-node-diff-status d)))))))
      (delete-directory left t)
      (delete-directory right t))))

(ert-deftest emeld-same-dir-hidden-when-same-off ()
  "Top-level `same'-only dirs hide when Same is off; changed dirs stay visible."
  (let* ((root (emeld-node-create :name "root" :is-dir t))
         (same-dir (emeld-node-create :name "samedir" :is-dir t :parent root
                                      :diff-status 'same))
         (diff-dir (emeld-node-create :name "diffdir" :is-dir t :parent root
                                      :diff-status 'different))
         (emeld--root-node root))
    (setf (emeld-node-children root) (list same-dir diff-dir))
    ;; Same on: structural context exposes the all-identical dir too.
    (let ((emeld-show-same t) (emeld-show-files t) (emeld-show-different t))
      (should (emeld--node-visible-p same-dir))
      (should (emeld--node-visible-p diff-dir)))
    ;; Same off: the all-identical dir disappears; the changed dir remains.
    (let ((emeld-show-same nil) (emeld-show-files t) (emeld-show-different t))
      (should-not (emeld--node-visible-p same-dir))
      (should (emeld--node-visible-p diff-dir)))))

(ert-deftest emeld-preset-savehist-registered ()
  "Loading savehist registers `emeld-presets' for cross-session persistence."
  (require 'savehist)
  (should (memq 'emeld-presets savehist-additional-variables)))

(ert-deftest emeld-delete-marked-side-aware ()
  "`emeld-delete' with marks deletes each node from its marked side(s)."
  (let* ((left (make-temp-file "emeld-left" t))
         (right (make-temp-file "emeld-right" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "sub" left))
          (dolist (f '("a.txt" "b.txt" "sub/c.txt"))
            (with-temp-file (expand-file-name f left) (insert "x")))
          (dolist (f '("a.txt" "d.txt"))
            (with-temp-file (expand-file-name f right) (insert "x")))
          (with-temp-buffer
            (setq-local emeld--left-root left
                        emeld--right-root right
                        emeld--column-width 40
                        emeld--marked-paths (make-hash-table :test #'equal))
            ;; a.txt marked on BOTH sides; b.txt + sub/c.txt on the left only;
            ;; d.txt unmarked (must survive).
            (emeld--set-mark "a.txt" 'left t)
            (emeld--set-mark "a.txt" 'right t)
            (emeld--set-mark "b.txt" 'left t)
            (emeld--set-mark "sub/c.txt" 'left t)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                      ((symbol-function 'emeld-refresh) (lambda () nil)))
              (emeld-delete))
            ;; Every marked side is deleted; the unmarked right d.txt survives.
            (should-not (file-exists-p (expand-file-name "a.txt" left)))
            (should-not (file-exists-p (expand-file-name "a.txt" right)))
            (should-not (file-exists-p (expand-file-name "b.txt" left)))
            (should-not (file-exists-p (expand-file-name "sub/c.txt" left)))
            (should (file-exists-p (expand-file-name "d.txt" right)))
            ;; Marks cleared after the operation.
            (should (= 0 (hash-table-count emeld--marked-paths)))))
      (delete-directory left t)
      (delete-directory right t))))

;;;; ─── Git revision comparison ──────────────────────────────────────

(ert-deftest emeld-parse-git-name-status-basic ()
  "The name-status parser maps M/A/D and splits renames into delete+add."
  (let* ((out (concat (string-join
                       '("M" "mod.txt"
                         "A" "added.txt"
                         "D" "gone.txt"
                         "R100" "old/name.txt" "new/name.txt"
                         "C75" "src.txt" "copy.txt")
                       "\0")
                      "\0"))
         (entries (emeld--parse-git-name-status out)))
    (should (eq 'different  (cdr (assoc "mod.txt" entries))))
    (should (eq 'right-only (cdr (assoc "added.txt" entries))))
    (should (eq 'left-only  (cdr (assoc "gone.txt" entries))))
    ;; A rename splits into the old path (deleted) plus the new path (added).
    (should (eq 'left-only  (cdr (assoc "old/name.txt" entries))))
    (should (eq 'right-only (cdr (assoc "new/name.txt" entries))))
    ;; A copy leaves the source untouched; only the new path is added.
    (should (eq 'right-only (cdr (assoc "copy.txt" entries))))
    (should-not (assoc "src.txt" entries))))

(ert-deftest emeld-git-rev-args-combos ()
  "`emeld--git-rev-args' builds the right `git diff' revision selectors."
  (should (equal (emeld--git-rev-args "HEAD~1" "HEAD") '("HEAD~1" "HEAD")))
  (should (equal (emeld--git-rev-args "HEAD" 'working) '("HEAD")))
  (should (equal (emeld--git-rev-args "HEAD" 'staged) '("--cached" "HEAD")))
  (should (null (emeld--git-rev-args 'staged 'working)))
  (should (null (emeld--git-rev-args 'working 'working)))
  ;; An unsupported orientation (working tree as the OLD side) errors.
  (should-error (emeld--git-rev-args 'working "HEAD")))

(ert-deftest emeld-git-ls-command-forms ()
  "`emeld--git-ls-command' lists a revision via ls-tree, the work tree via ls-files."
  (let ((emeld--git-repo "/repo/"))
    (should (equal (emeld--git-ls-command "HEAD")
                   '("git" "-C" "/repo" "ls-tree" "-r" "--name-only" "-z" "HEAD")))
    (should (equal (emeld--git-ls-command 'working)
                   '("git" "-C" "/repo" "ls-files" "-z")))
    (should (equal (emeld--git-ls-command 'staged)
                   '("git" "-C" "/repo" "ls-files" "-z")))))

(ert-deftest emeld-make-file-node-git-mode ()
  "In git mode file nodes carry the relpath and never probe the filesystem."
  (let ((emeld--git-mode t)
        (emeld--left-root "/repo/")
        (emeld--right-root "/repo/"))
    (let ((n (emeld--make-file-node "a/b.txt" nil 'different)))
      (should (equal (emeld-node-left-path n) "a/b.txt"))
      (should (equal (emeld-node-right-path n) "a/b.txt"))
      (should-not (emeld-node-is-dir n)))
    (let ((n (emeld--make-file-node "gone.txt" nil 'left-only)))
      (should (equal (emeld-node-left-path n) "gone.txt"))
      (should (null (emeld-node-right-path n)))
      (should-not (emeld-node-is-dir n)))
    (let ((n (emeld--make-file-node "new.txt" nil 'right-only)))
      (should (null (emeld-node-left-path n)))
      (should (equal (emeld-node-right-path n) "new.txt")))))

(ert-deftest emeld-git-tree-build ()
  "Git entries assemble into a tree of relpath-carrying nodes (no disk probe)."
  (let ((emeld--git-mode t)
        (emeld-filter-groups nil)
        (emeld--active-filter-groups nil)
        (emeld-respect-gitignore nil)
        (emeld--left-root "/repo/")
        (emeld--right-root "/repo/"))
    (let* ((root (emeld-node-create :name "root" :is-dir t))
           (entries '(("mod.txt" . different)
                      ("added.txt" . right-only)
                      ("gone.txt" . left-only)
                      ("sub/old.txt" . left-only)
                      ("sub/new.txt" . right-only)))
           (nodes (emeld--entries-to-nodes entries "" root))
           (by-name (lambda (nm) (cl-find nm nodes :key #'emeld-node-name :test #'string=))))
      (setf (emeld-node-children root) nodes)
      (should (eq 'different  (emeld-node-diff-status (funcall by-name "mod.txt"))))
      (should (eq 'right-only (emeld-node-diff-status (funcall by-name "added.txt"))))
      (should (eq 'left-only  (emeld-node-diff-status (funcall by-name "gone.txt"))))
      (let ((sub (funcall by-name "sub")))
        (should (emeld-node-is-dir sub))
        ;; A dir renders on both sides, carrying the relpath as its side path.
        (should (equal (emeld-node-left-path sub) "sub"))
        (should (equal (emeld-node-right-path sub) "sub"))
        (let ((kids (mapcar #'emeld-node-name (emeld-node-children sub))))
          (should (member "old.txt" kids))
          (should (member "new.txt" kids)))))))

(ert-deftest emeld-git-diff-name-status-real ()
  "End-to-end: a real `git diff --name-status' parses to the right entries."
  (skip-unless (executable-find "git"))
  (let ((dir (make-temp-file "emeld-git" t)))
    (unwind-protect
        (cl-flet ((g (&rest a) (apply #'call-process "git" nil nil nil "-C" dir a)))
          (g "init" "-q")
          (g "config" "user.email" "t@t.com")
          (g "config" "user.name" "T")
          ;; Commit 1.
          (with-temp-file (expand-file-name "keep.txt" dir) (insert "same\n"))
          (with-temp-file (expand-file-name "mod.txt" dir) (insert "one\n"))
          (with-temp-file (expand-file-name "gone.txt" dir) (insert "bye\n"))
          (make-directory (expand-file-name "sub" dir))
          (with-temp-file (expand-file-name "sub/old.txt" dir) (insert "rename me\n"))
          (g "add" "-A")
          (g "commit" "-q" "-m" "c1")
          ;; Commit 2: modify, delete, add, and rename within sub/.
          (with-temp-file (expand-file-name "mod.txt" dir) (insert "two\n"))
          (delete-file (expand-file-name "gone.txt" dir))
          (with-temp-file (expand-file-name "added.txt" dir) (insert "fresh\n"))
          (rename-file (expand-file-name "sub/old.txt" dir)
                       (expand-file-name "sub/new.txt" dir))
          (g "add" "-A")
          (g "commit" "-q" "-m" "c2")
          (with-temp-buffer
            (setq-local emeld--git-repo (file-name-as-directory dir)
                        emeld--git-left-rev "HEAD~1"
                        emeld--git-right-rev "HEAD")
            (let* ((cmd (emeld--git-diff-command))
                   (out (with-temp-buffer
                          (apply #'call-process (car cmd) nil t nil (cdr cmd))
                          (buffer-string)))
                   (entries (emeld--parse-git-name-status out)))
              (should (eq 'different  (cdr (assoc "mod.txt" entries))))
              (should (eq 'right-only (cdr (assoc "added.txt" entries))))
              (should (eq 'left-only  (cdr (assoc "gone.txt" entries))))
              ;; Unchanged files are never reported by `diff'.
              (should-not (assoc "keep.txt" entries))
              ;; The rename surfaces as a deleted old path and an added new path.
              (should (eq 'left-only  (cdr (assoc "sub/old.txt" entries))))
              (should (eq 'right-only (cdr (assoc "sub/new.txt" entries)))))))
      (delete-directory dir t))))

(ert-deftest emeld-git-diff-vs-working-tree ()
  "Comparing a commit to the working tree picks up uncommitted edits."
  (skip-unless (executable-find "git"))
  (let ((dir (make-temp-file "emeld-git" t)))
    (unwind-protect
        (cl-flet ((g (&rest a) (apply #'call-process "git" nil nil nil "-C" dir a)))
          (g "init" "-q")
          (g "config" "user.email" "t@t.com")
          (g "config" "user.name" "T")
          (with-temp-file (expand-file-name "a.txt" dir) (insert "v1\n"))
          (g "add" "-A")
          (g "commit" "-q" "-m" "c1")
          ;; Edit a tracked file without committing.
          (with-temp-file (expand-file-name "a.txt" dir) (insert "v2\n"))
          (with-temp-buffer
            (setq-local emeld--git-repo (file-name-as-directory dir)
                        emeld--git-left-rev "HEAD"
                        emeld--git-right-rev 'working)
            (let* ((cmd (emeld--git-diff-command))
                   (out (with-temp-buffer
                          (apply #'call-process (car cmd) nil t nil (cdr cmd))
                          (buffer-string)))
                   (entries (emeld--parse-git-name-status out)))
              (should (eq 'different (cdr (assoc "a.txt" entries)))))))
      (delete-directory dir t))))

(ert-deftest emeld-git-commit-info ()
  "`emeld--git-commit-info' parses commit metadata; non-commits return nil."
  (skip-unless (executable-find "git"))
  (let ((dir (make-temp-file "emeld-cm" t)))
    (unwind-protect
        (cl-flet ((g (&rest a) (apply #'call-process "git" nil nil nil "-C" dir a)))
          (g "init" "-q")
          (g "config" "user.email" "t@t.com")
          (g "config" "user.name" "Tester")
          (with-temp-file (expand-file-name "f.txt" dir) (insert "1\n"))
          (g "add" "-A")
          (g "commit" "-q" "-m" "Subject here" "-m" "Body A\nBody B")
          (with-temp-buffer
            (setq-local emeld--git-repo (file-name-as-directory dir))
            (let ((info (emeld--git-commit-info "HEAD")))
              (should (equal (plist-get info :subject) "Subject here"))
              (should (string-match-p "Body A" (plist-get info :body)))
              (should (string-match-p "Body B" (plist-get info :body)))
              (should (string-match-p "Tester" (plist-get info :author)))
              (should (stringp (plist-get info :sha)))
              (should (> (length (plist-get info :sha)) 0)))
            ;; The working tree and index are not commits.
            (should-not (emeld--git-commit-info 'working))
            (should-not (emeld--git-commit-info 'staged))))
      (delete-directory dir t))))

(ert-deftest emeld-git-history-list-and-parent ()
  "`emeld--git-history-list' is newest-first; the root's parent is the empty tree."
  (skip-unless (executable-find "git"))
  (let ((dir (make-temp-file "emeld-hist" t)))
    (unwind-protect
        (cl-flet ((g (&rest a) (apply #'call-process "git" nil nil nil "-C" dir a)))
          (g "init" "-q")
          (g "config" "user.email" "t@t.com")
          (g "config" "user.name" "T")
          (dotimes (k 4)
            (with-temp-file (expand-file-name "f.txt" dir) (insert (format "v%d\n" k)))
            (g "add" "-A")
            (g "commit" "-q" "-m" (format "c%d" k)))
          (with-temp-buffer
            (setq-local emeld--git-repo (file-name-as-directory dir))
            (let ((h (emeld--git-history-list "HEAD" 100)))
              (should (= 4 (length h)))
              ;; Newest first: HEAD's first parent is the second entry.
              (should (equal (emeld--git-parent-rev (nth 0 h)) (nth 1 h)))
              ;; The root commit (last) has the empty tree as its parent.
              (should (equal (emeld--git-parent-rev (nth 3 h)) emeld--git-empty-tree))
              ;; The limit caps how many commits are returned.
              (should (= 2 (length (emeld--git-history-list "HEAD" 2)))))))
      (delete-directory dir t))))

(ert-deftest emeld-git-history-apply-derives-pair ()
  "`emeld--git-history-apply' maps the index to (parent vs commit) and clamps."
  (let ((emeld--git-history '("aaa" "bbb" "ccc"))
        (emeld--git-history-index 0)
        (emeld--git-repo "/repo/")
        (emeld--git-left-rev nil)
        (emeld--git-right-rev nil)
        (emeld--root-node (emeld-node-create :name "" :is-dir t)))
    (cl-letf (((symbol-function 'emeld-refresh) (lambda () nil))
              ((symbol-function 'emeld--git-parent-rev)
               (lambda (_) emeld--git-empty-tree)))
      ;; Index 0: newest commit vs its parent (the next entry).
      (emeld--git-history-apply)
      (should (equal emeld--git-right-rev "aaa"))
      (should (equal emeld--git-left-rev "bbb"))
      ;; Last index: commit vs its real parent (the empty tree here).
      (setq emeld--git-history-index 2)
      (emeld--git-history-apply)
      (should (equal emeld--git-right-rev "ccc"))
      (should (equal emeld--git-left-rev emeld--git-empty-tree))
      ;; Out-of-range indices clamp into the list.
      (setq emeld--git-history-index 99)
      (emeld--git-history-apply)
      (should (= emeld--git-history-index 2)))))

(ert-deftest emeld-git-rev-label-abbreviates ()
  "Full SHAs abbreviate; the empty tree and named revs get readable labels."
  (should (equal (emeld--git-rev-label "0123456789abcdef0123456789abcdef01234567")
                 "01234567"))
  (should (equal (emeld--git-rev-label emeld--git-empty-tree) "∅ (root)"))
  (should (equal (emeld--git-rev-label "HEAD") "HEAD"))
  (should (equal (emeld--git-rev-label 'working) "working"))
  (should (equal (emeld--git-rev-label 'staged) "index")))

(message "Running tests...")
(ert-run-tests-batch-and-exit)
