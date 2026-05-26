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

(message "Running tests...")
(ert-run-tests-batch-and-exit)
