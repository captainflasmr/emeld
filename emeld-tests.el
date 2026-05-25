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

(message "Running tests...")
(ert-run-tests-batch-and-exit)
