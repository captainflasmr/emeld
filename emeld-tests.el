;;; emeld-tests.el --- Tests for emeld.el -*- lexical-binding: t; -*-

;; The package is now multi-file (emeld-core/-git/-sidebar, umbrella emeld.el).
;; Put the repo root on `load-path' so the parts resolve, then load the whole.
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name
                                                  default-directory)))
(require 'emeld)
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

(ert-deftest emeld-ensure-dir-locals-creates-defaults ()
  "A missing `.dir-locals.el' is created with the default filter groups."
  (let ((dir (make-temp-file "emeld-locals" t)))
    (unwind-protect
        (let ((file (expand-file-name dir-locals-file dir)))
          (should-not (file-exists-p file))
          (emeld--ensure-dir-locals (file-name-as-directory dir))
          (should (file-exists-p file))
          ;; The written value reads back as the defaults.
          (with-temp-buffer
            (insert-file-contents file)
            (let ((vars (cdar (read (current-buffer)))))
              (should (equal (cdr (assq 'emeld-filter-groups vars))
                             emeld-filter-groups)))))
      (delete-directory dir t))))

(ert-deftest emeld-ensure-dir-locals-keeps-existing ()
  "An existing `.dir-locals.el' is never overwritten."
  (let ((dir (make-temp-file "emeld-locals" t)))
    (unwind-protect
        (let ((file (expand-file-name dir-locals-file dir)))
          (with-temp-file file (insert "((nil . ((some-var . 1))))\n"))
          (emeld--ensure-dir-locals (file-name-as-directory dir))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (read (current-buffer))
                           '((nil . ((some-var . 1))))))))
      (delete-directory dir t))))

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

(ert-deftest emeld-file-meta-string ()
  "`emeld--file-meta-string' annotates existing files only, in dir mode."
  (let ((f (make-temp-file "emeld-meta" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file f (insert "hello"))
          ;; Off by default.
          (let ((emeld-show-file-meta nil) (emeld--git-mode nil))
            (should-not (emeld--file-meta-string f nil)))
          ;; On: size tag with the meta face.
          (let ((emeld-show-file-meta t) (emeld--git-mode nil))
            (let ((s (emeld--file-meta-string f nil)))
              (should (string-match-p "(5 " s))
              (should (eq (get-text-property 0 'face s) 'emeld-meta-face)))
            ;; Directories, missing paths, and git mode give nil.
            (should-not (emeld--file-meta-string f t))
            (should-not (emeld--file-meta-string "/nonexistent-emeld" nil)))
          (let ((emeld-show-file-meta t) (emeld--git-mode t))
            (should-not (emeld--file-meta-string f nil))))
      (delete-file f))))

(ert-deftest emeld-render-file-meta ()
  "Rendered file rows carry per-side size tags when enabled."
  (let ((l (make-temp-file "emeld-meta-l" nil ".txt"))
        (r (make-temp-file "emeld-meta-r" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file l (insert "twelve-bytes!"))
          (with-temp-file r (insert "six!"))
          (with-temp-buffer
            (setq-local emeld--marked-paths (make-hash-table :test #'equal))
            (setq-local emeld--git-mode nil)
            (let ((emeld-show-file-meta t)
                  (emeld-show-strikethrough t)
                  (node (emeld-node-create :name "f.txt" :left-path l
                                           :right-path r
                                           :diff-status 'different)))
              (emeld--render-nodes (list node) "" 60)
              (should (string-match-p "(13 " (buffer-string)))
              (should (string-match-p "(4 " (buffer-string))))))
      (delete-file l)
      (delete-file r))))

(ert-deftest emeld-file-meta-cached-across-renders ()
  "A node's file-meta tag is computed once and reused by later renders."
  (let* ((file (make-temp-file "emeld-meta" nil ".txt"))
         (calls 0)
         (real (symbol-function 'file-attributes)))
    (unwind-protect
        (with-temp-buffer
          (setq-local emeld--marked-paths (make-hash-table :test #'equal))
          (setq-local emeld--git-mode nil)
          (let ((emeld-show-file-meta t)
                (node (emeld-node-create :name (file-name-nondirectory file)
                                         :diff-status 'different
                                         :left-path file :right-path file)))
            (cl-letf (((symbol-function 'file-attributes)
                       (lambda (path &optional id-format)
                         (cl-incf calls)
                         (funcall real path id-format))))
              (emeld--render-nodes (list node) "" 60)
              (let ((first calls)
                    (first-text (buffer-string)))
                ;; Both sides are tagged on the first render (two stats).
                (should (= 2 first))
                (erase-buffer)
                (emeld--render-nodes (list node) "" 60)
                ;; The second render is served entirely from the cache.
                (should (= 2 calls))
                (should (equal first-text (buffer-string)))))))
      (delete-file file))))

(ert-deftest emeld-file-meta-toggle-live ()
  "Toggling `emeld-show-file-meta' updates rendered rows at once, no rescan."
  (let* ((file (make-temp-file "emeld-meta-t" nil ".txt"))
         (calls 0)
         (real (symbol-function 'file-attributes)))
    (unwind-protect
        (with-temp-buffer
          (setq-local emeld--marked-paths (make-hash-table :test #'equal))
          (setq-local emeld--git-mode nil)
          (let ((emeld-show-file-meta nil)
                (node (emeld-node-create :name (file-name-nondirectory file)
                                         :diff-status 'different
                                         :left-path file :right-path file)))
            (cl-letf (((symbol-function 'file-attributes)
                       (lambda (path &optional id-format)
                         (cl-incf calls)
                         (funcall real path id-format))))
              ;; Off: no tag, and nothing is stat'ed or cached.
              (emeld--render-nodes (list node) "" 60)
              (should-not (string-match-p "(" (buffer-string)))
              (should (= 0 calls))
              ;; Toggle on: the tag appears on this very render.
              (setq emeld-show-file-meta t)
              (erase-buffer)
              (emeld--render-nodes (list node) "" 60)
              (should (string-match-p "(" (buffer-string)))
              (should (= 2 calls))
              ;; Toggle off: it disappears again, still without a rescan.
              (setq emeld-show-file-meta nil)
              (erase-buffer)
              (emeld--render-nodes (list node) "" 60)
              (should-not (string-match-p "(" (buffer-string))))))
      (delete-file file))))

(ert-deftest emeld-reclassify-drops-meta-cache ()
  "Re-classifying after a copy drops the node's cached meta tags."
  (let* ((left (make-temp-file "emeld-metaL" t))
         (right (make-temp-file "emeld-metaR" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.txt" left) (insert "same"))
          (with-temp-file (expand-file-name "a.txt" right) (insert "same"))
          (with-temp-buffer
            (setq-local emeld--left-root left
                        emeld--right-root right
                        emeld--marked-paths (make-hash-table :test #'equal))
            (setq-local emeld--git-mode nil)
            (let ((node (emeld-node-create :name "a.txt" :diff-status 'different
                                           :left-path (expand-file-name "a.txt" left)
                                           :right-path (expand-file-name "a.txt" right))))
              (setf (emeld-node-left-meta node) " (4 B 01-01 00:00)")
              (emeld--reclassify-copied-node node)
              (should-not (emeld-node-left-meta node))
              (should-not (emeld-node-right-meta node))
              (should (eq 'same (emeld-node-diff-status node))))))
      (delete-directory left t)
      (delete-directory right t))))

(ert-deftest emeld-swap-panes-rotates-meta-cache ()
  "Swapping panes carries cached meta tags over with their paths."
  (with-temp-buffer
    (setq-local emeld--git-mode nil)
    (setq-local emeld--left-root "/x/" emeld--right-root "/y/")
    (let ((node (emeld-node-create :name "f.txt" :diff-status 'different
                                   :left-path "/x/f.txt" :right-path "/y/f.txt"))
          (root (emeld-node-create :name "root" :is-dir t :expanded t)))
      (setf (emeld-node-children root) (list node)
            (emeld-node-parent node) root
            emeld--root-node root
            (emeld-node-left-meta node) " L"
            (emeld-node-right-meta node) " R")
      (cl-letf (((symbol-function 'emeld--render) (lambda () nil)))
        (emeld-swap-panes))
      (should (equal "/y/f.txt" (emeld-node-left-path node)))
      (should (equal "/x/f.txt" (emeld-node-right-path node)))
      (should (equal " R" (emeld-node-left-meta node)))
      (should (equal " L" (emeld-node-right-meta node))))))

(ert-deftest emeld-status-marker-lookup ()
  "Markers default to M/A/D; dirs and unlisted statuses get none."
  (should (equal "M " (emeld--status-marker
                       (emeld-node-create :name "f" :diff-status 'different))))
  (should (equal "A " (emeld--status-marker
                       (emeld-node-create :name "f" :diff-status 'left-only))))
  (should (equal "D " (emeld--status-marker
                       (emeld-node-create :name "f" :diff-status 'right-only))))
  (should (equal "" (emeld--status-marker
                     (emeld-node-create :name "f" :diff-status 'same))))
  ;; Directories never get a marker, even with a matching status.
  (should (equal "" (emeld--status-marker
                     (emeld-node-create :name "d" :is-dir t
                                        :diff-status 'different))))
  ;; A custom alist restyles, opts `same' in and disables others.
  (let ((emeld-status-markers '((same . "= "))))
    (should (equal "= " (emeld--status-marker
                         (emeld-node-create :name "f" :diff-status 'same))))
    (should (equal "" (emeld--status-marker
                       (emeld-node-create :name "f" :diff-status 'different))))))

(ert-deftest emeld-render-status-markers ()
  "Rendered rows carry the status marker before the name on both sides."
  (with-temp-buffer
    (setq-local emeld--marked-paths (make-hash-table :test #'equal))
    (setq-local emeld--git-mode nil)
    (let ((emeld-show-file-meta nil)
          (emeld-show-strikethrough t)
          (nodes (list (emeld-node-create :name "m.txt" :left-path "/x/m.txt"
                                          :right-path "/x/m.txt"
                                          :diff-status 'different)
                       (emeld-node-create :name "l.txt" :left-path "/x/l.txt"
                                          :diff-status 'left-only)
                       (emeld-node-create :name "r.txt" :right-path "/x/r.txt"
                                          :diff-status 'right-only))))
      (emeld--render-nodes nodes "" 80)
      (let ((text (buffer-string)))
        (dolist (pattern '("M m\\.txt" "A l\\.txt" "D r\\.txt"))
          (let ((n 0) (pos 0))
            (while (string-match pattern text pos)
              (setq n (1+ n) pos (match-end 0)))
            (should (equal 2 n))))))
    ;; Disabling the markers leaves the file names bare.
    (erase-buffer)
    (let ((emeld-status-markers nil)
          (node (emeld-node-create :name "f.txt" :left-path "/x/f.txt"
                                   :right-path "/x/f.txt"
                                   :diff-status 'different)))
      (emeld--render-nodes (list node) "" 60)
      (should-not (string-match-p "M f\\.txt" (buffer-string))))))

(ert-deftest emeld-render-inline-diff-tree-guides ()
  "Inline diff lines repeat the node's trunk, in the rows' `shadow' face."
  (with-temp-buffer
    (setq-local emeld--marked-paths (make-hash-table :test #'equal))
    (setq-local emeld--git-mode nil)
    (let* ((emeld-show-file-meta nil)
           (a (emeld-node-create :name "a.txt" :diff-status 'different
                                 :left-path "/x/sub/a.txt" :right-path "/x/sub/a.txt"
                                 :inline-diff (list "+added")))
           (b (emeld-node-create :name "b.txt" :diff-status 'different
                                 :left-path "/x/sub/b.txt" :right-path "/x/sub/b.txt"
                                 :inline-diff (list "-gone")))
           (c (emeld-node-create :name "c.txt" :diff-status 'different
                                 :left-path "/x/c.txt" :right-path "/x/c.txt"
                                 :inline-diff (list "+root")))
           (d (emeld-node-create :name "d.txt" :diff-status 'different
                                 :left-path "/x/d.txt" :right-path "/x/d.txt"))
           (sub (emeld-node-create :name "sub" :is-dir t :diff-status 'different
                                   :left-path "/x/sub" :right-path "/x/sub"
                                   :children (list a b)))
           (root (emeld-node-create :name "" :is-dir t :expanded t
                                    :children (list sub c d))))
      (setf (emeld-node-parent sub) root
            (emeld-node-parent c) root
            (emeld-node-parent d) root
            (emeld-node-parent a) sub
            (emeld-node-parent b) sub)
      (emeld--render-nodes (emeld-node-children root) "" 80)
      ;; a.txt is nested and not the last sibling: the preview repeats the
      ;; parent's guide and a.txt's own trunk, both shadow-faced like the rows.
      (goto-char (point-min))
      (search-forward "+added")
      (let ((bol (line-beginning-position)))
        (should (equal "  │  │   " (buffer-substring-no-properties bol (+ bol 9))))
        (should (eq ?│ (char-after (+ bol 5))))
        (should (eq 'shadow (get-text-property (+ bol 2) 'face)))
        (should (eq 'shadow (get-text-property (+ bol 5) 'face))))
      ;; b.txt is the last sibling: its trunk column stays blank.
      (goto-char (point-min))
      (search-forward "-gone")
      (let ((bol (line-beginning-position)))
        (should (equal "  │      " (buffer-substring-no-properties bol (+ bol 9)))))
      ;; c.txt is top level and not the last sibling: a single trunk glyph.
      (goto-char (point-min))
      (search-forward "+root")
      (let ((bol (line-beginning-position)))
        (should (equal "  │   " (buffer-substring-no-properties bol (+ bol 6))))
        (should (eq 'shadow (get-text-property (+ bol 2) 'face))))
      ;; With tree lines disabled the preview stays blank, like the rows.
      (let ((emeld-show-tree-lines nil))
        (erase-buffer)
        (emeld--render-nodes (emeld-node-children root) "" 80)
        (goto-char (point-min))
        (search-forward "+added")
        (should-not (string-match-p
                     "│" (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))))))

(ert-deftest emeld-ensure-diff-program-signals-when-missing ()
  "A missing `diff' program fails fast with a friendly user error."
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (should-error (emeld--ensure-diff-program) :type 'user-error)
    ;; `emeld-diff' checks before creating a buffer or a recent entry.
    (should-error (emeld-diff "/tmp/emeld-no-diff-a" "/tmp/emeld-no-diff-b")
                  :type 'user-error))
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/usr/bin/diff")))
    (should-not (emeld--ensure-diff-program))))

(ert-deftest emeld-refresh-requires-diff-in-dir-mode ()
  "Directory rescans fail before touching scan state when `diff' is missing."
  (with-temp-buffer
    (setq-local emeld--git-mode nil
                emeld--root-node (emeld-node-create :name "root" :is-dir t :expanded t))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
      (should-error (emeld-refresh) :type 'user-error))))

(ert-deftest emeld-window-size-change-schedules-resize-render ()
  "A resize that changes the pane split schedules a debounced re-render."
  (with-temp-buffer
    (setq-local emeld--root-node (emeld-node-create :name "root" :is-dir t :expanded t))
    (unwind-protect
        (progn
          (let ((split (/ (* (window-width (selected-window)) emeld-column-ratio) 100)))
            ;; A stale split, as after a drag: a render is scheduled.
            (setq emeld--column-width (1+ split))
            (emeld--window-size-changed (selected-window))
            (should (timerp emeld--resize-render-timer))
            ;; A width mapping to the same split schedules nothing.
            (emeld--cancel-resize-render)
            (setq emeld--column-width split)
            (emeld--window-size-changed (selected-window))
            (should-not emeld--resize-render-timer))
          ;; No comparison loaded yet: nothing to render.
          (setq emeld--root-node nil
                emeld--column-width nil)
          (emeld--window-size-changed (selected-window))
          (should-not emeld--resize-render-timer)
          ;; The timer callback re-renders only while the split is stale.
          (let ((rendered 0))
            (cl-letf (((symbol-function 'emeld--render)
                       (lambda () (cl-incf rendered)))
                      ((symbol-function 'get-buffer-window)
                       (lambda (&rest _) (selected-window))))
              (setq emeld--column-width 9999)
              (emeld--resize-render (current-buffer))
              (should (equal 1 rendered))
              (setq emeld--column-width
                    (/ (* (window-width (selected-window)) emeld-column-ratio) 100))
              (emeld--resize-render (current-buffer))
              (should (equal 1 rendered)))))
      (emeld--cancel-resize-render))))

(ert-deftest emeld-resize-render-reflows-pane-split ()
  "The resize callback re-renders at the window's new width.
The pane split is computed once per `emeld--render' and baked into the
rows' padding, so the callback must re-render and refresh
`emeld--column-width' for the two sides to line up again."
  (let* ((frame (selected-frame))
         (win (selected-window))
         (width (frame-width frame))
         (height (frame-height frame))
         (old-buf (window-buffer win))
         (root (emeld-node-create :name "root" :is-dir t :expanded t))
         (buf (get-buffer-create " *emeld-resize-render-test*")))
    (unwind-protect
        (progn
          (setf (emeld-node-children root)
                (list (emeld-node-create :name "a.txt" :diff-status 'different
                                         :left-path "/x/a.txt" :right-path "/y/a.txt"
                                         :parent root)))
          (with-current-buffer buf
            (emeld-mode)
            (setq emeld--status-phase 'done
                  emeld--marked-paths (make-hash-table :test #'equal)
                  emeld--hidden-paths nil
                  emeld--left-root "/x/"
                  emeld--right-root "/y/"
                  emeld-show-file-meta nil
                  emeld-show-same t
                  emeld-show-files t
                  emeld-show-added t
                  emeld-show-different t
                  emeld--git-mode nil
                  emeld--root-node root))
          (set-window-buffer win buf)
          (with-current-buffer buf
            (emeld--render)
            (set-frame-size frame (- width 20) height)
            (skip-unless (/= (window-width win) width))
            (should (emeld--split-outdated-p win))
            (emeld--resize-render buf)
            (should-not (emeld--split-outdated-p win))
            (should (= emeld--column-width
                       (/ (* (window-width win) emeld-column-ratio) 100)))))
      (set-frame-size frame width height)
      (set-window-buffer win old-buf)
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest emeld-content-filter-args ()
  "Content filters assemble into flags; the case filter is dir-mode only."
  (let ((emeld-ignore-whitespace nil)
        (emeld-ignore-blank-lines nil)
        (emeld-ignore-case nil)
        (emeld-ignore-matching-lines nil))
    (should-not (emeld--content-filter-args))
    (should-not (emeld--content-filter-args t))
    (setq emeld-ignore-whitespace 'change)
    (should (equal '("-b") (emeld--content-filter-args)))
    (setq emeld-ignore-whitespace 'all
          emeld-ignore-blank-lines t
          emeld-ignore-matching-lines "^#"
          emeld-ignore-case t)
    (should (equal '("-w" "--ignore-blank-lines" "--ignore-matching-lines=^#" "-i")
                   (emeld--content-filter-args)))
    ;; `git diff' has no case-insensitive mode, so -i is dropped there.
    (should (equal '("-w" "--ignore-blank-lines" "--ignore-matching-lines=^#")
                   (emeld--content-filter-args t)))))

(ert-deftest emeld-content-filters-change-scan ()
  "Ignored-only differences drop out of a one-level scan when filters are on."
  (let ((left (make-temp-file "emeld-fl" t))
        (right (make-temp-file "emeld-fr" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "case.txt" left) (insert "ALPHA\n"))
          (with-temp-file (expand-file-name "case.txt" right) (insert "alpha\n"))
          (with-temp-file (expand-file-name "blank.txt" left) (insert "one\ntwo\n"))
          (with-temp-file (expand-file-name "blank.txt" right) (insert "one\n\ntwo\n"))
          (with-temp-file (expand-file-name "re.txt" left) (insert "x\n# note\ny\n"))
          (with-temp-file (expand-file-name "re.txt" right) (insert "x\ny\n"))
          (with-temp-file (expand-file-name "real.txt" left) (insert "hello\n"))
          (with-temp-file (expand-file-name "real.txt" right) (insert "world\n"))
          (let ((emeld-respect-gitignore nil)
                (emeld--active-filter-groups nil)
                (emeld-ignore-whitespace nil)
                (emeld-ignore-blank-lines nil)
                (emeld-ignore-case nil)
                (emeld-ignore-matching-lines nil))
            (should (= 4 (length (emeld--diff-level-entries left right))))
            (setq emeld-ignore-case t
                  emeld-ignore-blank-lines t
                  emeld-ignore-matching-lines "^#")
            (let ((filtered (emeld--diff-level-entries left right)))
              (should (equal '("real.txt" . different) (car filtered)))
              (should (= 1 (length filtered))))))
      (delete-directory left t)
      (delete-directory right t))))

(ert-deftest emeld-change-count-render ()
  "The render tallies visible changed files for the header count."
  (with-temp-buffer
    (setq-local emeld--status-phase 'done
                emeld--marked-paths (make-hash-table :test #'equal)
                emeld--hidden-paths nil
                emeld--left-root "/tmp/x/"
                emeld--right-root "/tmp/y/"
                emeld--column-width 40
                emeld-show-file-meta nil
                emeld-show-same nil
                emeld-show-files t
                emeld-show-added t
                emeld-show-different t
                emeld--git-mode nil)
    (let* ((root (emeld-node-create :name "root" :is-dir t :expanded t))
           (mod  (emeld-node-create :name "m.txt" :parent root
                                    :left-path "/x/m.txt" :right-path "/x/m.txt"
                                    :diff-status 'different))
           (new  (emeld-node-create :name "n.txt" :parent root
                                    :left-path "/x/n.txt"
                                    :diff-status 'left-only))
           (same (emeld-node-create :name "s.txt" :parent root
                                    :left-path "/x/s.txt" :right-path "/x/s.txt"
                                    :diff-status 'same))
           (sub  (emeld-node-create :name "sub" :is-dir t :parent root :expanded t
                                    :diff-status 'different))
           (del  (emeld-node-create :name "d.txt" :parent sub
                                    :right-path "/x/sub/d.txt"
                                    :diff-status 'right-only)))
      (setf (emeld-node-children sub) (list del))
      (setf (emeld-node-children root) (list mod new same sub))
      (setq-local emeld--root-node root)
      (emeld--render)
      ;; m.txt, n.txt and sub/d.txt count; the same file does not.
      (should (= 3 emeld--change-count))
      ;; Session-hidden nodes are not drawn, so they stop counting.
      (setq-local emeld--hidden-paths '("m.txt"))
      (emeld--render)
      (should (= 2 emeld--change-count)))))

(ert-deftest emeld-change-count-string ()
  "The header count is bold, pluralized and suppressed during scans."
  (with-temp-buffer
    (setq-local emeld--change-count 4
                emeld--status-phase 'done)
    (let ((s (emeld--change-count-string)))
      (should (string-match-p "\\`4 changes " s))
      (should (eq 'bold (get-text-property 0 'face s))))
    (setq-local emeld--change-count 1)
    (should (string-match-p "\\`1 change " (emeld--change-count-string)))
    (setq-local emeld--status-phase 'running-diff)
    (should-not (emeld--change-count-string))
    (setq-local emeld--status-phase 'building-tree)
    (should-not (emeld--change-count-string))))

(ert-deftest emeld-change-navigation ()
  "Change movement skips directories, identical files and inline diff lines."
  (with-temp-buffer
    (setq-local emeld--status-phase 'done
                emeld--marked-paths (make-hash-table :test #'equal)
                emeld--hidden-paths nil
                emeld--left-root "/tmp/x/"
                emeld--right-root "/tmp/y/"
                emeld-show-file-meta nil
                emeld-show-same t
                emeld-show-files t
                emeld-show-added t
                emeld-show-different t
                emeld--git-mode nil)
    (let* ((root  (emeld-node-create :name "root" :is-dir t :expanded t))
           (same  (emeld-node-create :name "a-same.txt" :parent root
                                     :left-path "/x/a" :right-path "/x/a"
                                     :diff-status 'same))
           (dir   (emeld-node-create :name "b-dir" :is-dir t :parent root :expanded t
                                     :diff-status 'different))
           (inner (emeld-node-create :name "inner.txt" :parent dir
                                     :left-path "/x/i" :right-path "/x/i"
                                     :diff-status 'different))
           (mod   (emeld-node-create :name "c-mod.txt" :parent root
                                     :left-path "/x/c" :right-path "/x/c"
                                     :diff-status 'different))
           (new   (emeld-node-create :name "d-new.txt" :parent root
                                     :left-path "/x/d"
                                     :diff-status 'left-only)))
      (setf (emeld-node-inline-diff mod) (list "  @@ -1 +1 @@"))
      (setf (emeld-node-children dir) (list inner))
      (setf (emeld-node-children root) (list same dir mod new))
      (setq-local emeld--root-node root)
      (emeld--render)
      (goto-char (point-min))
      ;; First change: inner.txt (same file and directory rows skipped).
      (emeld-next-change)
      (should (equal "inner.txt" (emeld-node-name (emeld--current-node))))
      (emeld-next-change)
      (should (equal "c-mod.txt" (emeld-node-name (emeld--current-node))))
      ;; The inline diff line under c-mod.txt is not a stop.
      (emeld-next-change)
      (should (equal "d-new.txt" (emeld-node-name (emeld--current-node))))
      ;; No further change: point stays put.
      (emeld-next-change)
      (should (equal "d-new.txt" (emeld-node-name (emeld--current-node))))
      (emeld-previous-change)
      (should (equal "c-mod.txt" (emeld-node-name (emeld--current-node)))))))

(ert-deftest emeld-delete-one-captures-trash ()
  "`emeld--delete-one' returns the trash location it moved PATH to."
  (let* ((tmp (make-temp-file "emeld-trash" t))
         (trash-directory (expand-file-name "TRASH" tmp))
         (f (expand-file-name "v.txt" tmp)))
    (unwind-protect
        (progn
          (with-temp-file f (insert "x"))
          (let ((trash (emeld--delete-one f)))
            (should-not (file-exists-p f))
            (should (stringp trash))
            (should (file-exists-p trash))))
      (delete-directory tmp t))))

(ert-deftest emeld-delete-at-point-logs-and-undo-restores ()
  "Deleting at point logs a restorable entry; undo brings the file back."
  (let* ((left (make-temp-file "emeldL" t))
         (right (make-temp-file "emeldR" t))
         (tmp (make-temp-file "emeld-trash" t))
         (trash-directory (expand-file-name "TRASH" tmp)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "v.txt" left) (insert "data"))
          (with-temp-buffer
            (setq-local emeld--left-root left
                        emeld--right-root right
                        emeld--column-width 40
                        emeld--marked-paths (make-hash-table :test #'equal)
                        emeld--op-log nil)
            (let ((node (emeld-node-create :name "v.txt"
                                           :left-path (expand-file-name "v.txt" left)
                                           :right-path nil))
                  (refreshed 0))
              (insert (propertize "v" 'emeld-node node))
              (goto-char (point-min))
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                        ((symbol-function 'emeld-refresh)
                         (lambda (&rest _) (setq refreshed (1+ refreshed)))))
                (emeld-delete-at-point)
                (should-not (file-exists-p (expand-file-name "v.txt" left)))
                (should (= 1 (length emeld--op-log)))
                (let ((before refreshed))
                  (emeld-undo-last-op)
                  ;; Undoing a delete rescans the whole comparison so the
                  ;; restored file shows up.
                  (should (> refreshed before)))
                (should (file-exists-p (expand-file-name "v.txt" left)))
                (should (equal "data" (with-temp-buffer
                                        (insert-file-contents (expand-file-name "v.txt" left))
                                        (buffer-string))))
                (should (null emeld--op-log))))))
      (delete-directory left t)
      (delete-directory right t)
      (delete-directory tmp t))))

(ert-deftest emeld-undo-copy-removes-dest ()
  "Undoing a copy entry trashes the copied destination."
  (let* ((tmp (make-temp-file "emeld-undo" t))
         (trash-directory (expand-file-name "TRASH" tmp))
         (dest (expand-file-name "copied.txt" tmp)))
    (unwind-protect
        (progn
          (with-temp-file dest (insert "copied"))
          (with-temp-buffer
            (setq-local emeld--left-root tmp
                        emeld--right-root tmp
                        emeld--op-log (list (list :type 'copy :src "/elsewhere/a.txt"
                                                 :dest dest :time (current-time)
                                                 :is-dir nil :mirror nil)))
            (cl-letf (((symbol-function 'emeld-refresh) (lambda () nil)))
              (emeld-undo-last-op)
              (should-not (file-exists-p dest))
              (should (null emeld--op-log)))))
      (delete-directory tmp t))))

(ert-deftest emeld-undo-empty-errors ()
  "Undo with an empty log signals an error."
  (with-temp-buffer
    (setq-local emeld--op-log nil)
    (should-error (emeld-undo-last-op))))

(defmacro emeld--with-temp-recent (&rest body)
  "Run BODY with `emeld-recent' isolated, persisting to a temp file."
  (declare (indent 0) (debug t))
  `(let* ((emeld-recent-file (make-temp-file "emeld-recent"))
          (emeld-recent nil))
     (unwind-protect (progn ,@body)
       (delete-file emeld-recent-file))))

(ert-deftest emeld-recent-push-dedupes-and-caps ()
  "Recent pushes move to front, drop duplicates, and honor the cap."
  (emeld--with-temp-recent
    (let ((emeld-recent-max 3))
      (emeld--recent-push '(dir "/a" "/b" nil))
      (emeld--recent-push '(dir "/c" "/d" nil))
      (emeld--recent-push '(dir "/a" "/b" nil))
      (should (equal emeld-recent '((dir "/a" "/b" nil) (dir "/c" "/d" nil))))
      (emeld--recent-push '(dir "/e" "/f" nil))
      (emeld--recent-push '(dir "/g" "/h" nil))
      (should (= 3 (length emeld-recent)))
      (should (equal (car emeld-recent) '(dir "/g" "/h" nil))))
    ;; Pushes are written through to the persistence file immediately.
    (with-temp-buffer
      (insert-file-contents emeld-recent-file)
      (goto-char (point-min))
      (should (equal (read (current-buffer)) emeld-recent)))))

(ert-deftest emeld-recent-valid-and-label ()
  "Validity drops missing dirs; labels describe each entry kind."
  (let ((left (make-temp-file "emeldL" t))
        (right (make-temp-file "emeldR" t)))
    (unwind-protect
        (progn
          (should (emeld--recent-valid-p (list 'dir left right nil)))
          (should-not (emeld--recent-valid-p (list 'dir left "/nonexistent-emeld")))
          (should (emeld--recent-valid-p (list 'git left "HEAD" 'working)))
          (should-not (emeld--recent-valid-p '(nonsense)))
          (should (string-match-p "<->" (emeld--recent-label (list 'dir left right nil))))
          (should (string-match-p "working" (emeld--recent-label (list 'git left "HEAD" 'working))))
          (should (string-match-p "history from" (emeld--recent-label (list 'history left "HEAD")))))
      (delete-directory left t)
      (delete-directory right t))))

(ert-deftest emeld-recent-open-dispatches ()
  "Reopening routes dir/git/history entries to their commands."
  (let (called)
    (cl-letf (((symbol-function 'emeld-diff)
               (lambda (&rest args) (setq called (cons 'diff args))))
              ((symbol-function 'emeld-git-diff)
               (lambda (&rest args) (setq called (cons 'git args))))
              ((symbol-function 'emeld-git-step-history)
               (lambda (&rest args) (setq called (cons 'hist args)))))
      (emeld--recent-open '(dir "/l" "/r" nil))
      (should (equal called '(diff "/l" "/r" nil)))
      (emeld--recent-open '(git "/repo" "HEAD~1" "HEAD"))
      (should (equal called '(git "/repo" "HEAD~1" "HEAD")))
      (emeld--recent-open '(history "/repo" "HEAD"))
      (should (equal called '(hist "/repo" "HEAD"))))))

(ert-deftest emeld-diff-pushes-recent ()
  "Starting a comparison records it in `emeld-recent' and persists it."
  (emeld--with-temp-recent
    (let ((left (make-temp-file "emeldL" t))
          (right (make-temp-file "emeldR" t)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'emeld-refresh) (lambda () nil)))
              (emeld-diff left right))
            (should (= 1 (length emeld-recent)))
            (should (eq 'dir (caar emeld-recent)))
            ;; The entry is on disk straight away.
            (with-temp-buffer
              (insert-file-contents emeld-recent-file)
              (goto-char (point-min))
              (should (equal (read (current-buffer)) emeld-recent))))
        (dolist (b (buffer-list))
          (when (string-prefix-p "*emeld: " (buffer-name b))
            (kill-buffer b)))
        (delete-directory left t)
        (delete-directory right t)))))

(ert-deftest emeld-recent-empty-errors ()
  "No valid recents signals an error instead of prompting."
  (let ((emeld-recent nil))
    (should-error (emeld-recent))))

(ert-deftest emeld-recent-save-load-roundtrip ()
  "`emeld-recent' is written to and read back from `emeld-recent-file'."
  (let* ((emeld-recent-file (make-temp-file "emeld-recent"))
         (entries '((dir "/a" "/b" 2)
                    (git "/repo" "HEAD" working)
                    (history "/repo" "HEAD")))
         (emeld-recent entries))
    (unwind-protect
        (progn
          (emeld--recent-save)
          (let ((emeld-recent nil))
            (emeld--recent-load t)
            (should (equal emeld-recent entries))))
      (delete-file emeld-recent-file))))

(ert-deftest emeld-recent-load-filters-and-caps ()
  "Loading drops malformed entries and honors `emeld-recent-max'."
  (let* ((emeld-recent-file (make-temp-file "emeld-recent"))
         (emeld-recent-max 2)
         (emeld-recent '((dir "/a" "/b" nil)
                         (bogus "/nope")
                         (dir "/c" "/d" nil)
                         (dir "/e" "/f" nil))))
    (unwind-protect
        (progn
          (emeld--recent-save)
          (let ((emeld-recent nil))
            (emeld--recent-load t)
            (should (equal emeld-recent '((dir "/a" "/b" nil)
                                          (dir "/c" "/d" nil))))))
      (delete-file emeld-recent-file))))

(ert-deftest emeld-mark-dir-from-dired ()
  "Marking in dired records that buffer's directory as side A.
Comparing from another dired buffer pairs it with that directory."
  (let* ((tmp-a (make-temp-file "emeld-marks-a" t))
         (tmp-b (make-temp-file "emeld-marks-b" t))
         (emeld-dir-a nil)
         (dired-a nil)
         (dired-b nil))
    (unwind-protect
        (progn
          (setq dired-a (dired-noselect tmp-a)
                dired-b (dired-noselect tmp-b))
          (with-current-buffer dired-a (emeld-mark-dir-a))
          (should (equal emeld-dir-a (directory-file-name (expand-file-name tmp-a))))
          (let (captured)
            (cl-letf (((symbol-function 'emeld-diff)
                       (lambda (&rest args) (setq captured args))))
              (with-current-buffer dired-b (emeld-compare-marked-dirs 2)))
            (should (equal captured
                           (list (directory-file-name (expand-file-name tmp-a))
                                 (directory-file-name (expand-file-name tmp-b))
                                 2)))))
      (dolist (buf (list dired-a dired-b))
        (when (buffer-live-p buf) (kill-buffer buf)))
      (delete-directory tmp-a t)
      (delete-directory tmp-b t))))

(ert-deftest emeld-mark-dir-uses-default-directory ()
  "Outside dired, marking records the buffer's `default-directory'."
  (let ((emeld-dir-a nil))
    (with-temp-buffer
      (setq default-directory "/marked-a/")
      (emeld-mark-dir-a))
    (should (equal emeld-dir-a "/marked-a"))))

(ert-deftest emeld-compare-marked-dirs-dispatch ()
  "Comparing calls `emeld-diff' with A, the current directory and depth."
  (let ((emeld-dir-a "/a")
        captured)
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (d) (member d '("/a" "/b"))))
              ((symbol-function 'emeld-diff)
               (lambda (&rest args) (setq captured args))))
      (with-temp-buffer
        (setq default-directory "/b/")
        (emeld-compare-marked-dirs 3)))
    (should (equal captured '("/a" "/b" 3)))))

(ert-deftest emeld-compare-marked-dirs-validates ()
  "No mark, same directory, or a stale mark all error."
  (let ((emeld-dir-a nil))
    (should-error (emeld-compare-marked-dirs) :type 'user-error))
  (let ((emeld-dir-a "/x"))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t)))
      (with-temp-buffer
        (setq default-directory "/x/")
        (should-error (emeld-compare-marked-dirs) :type 'user-error))))
  (let ((emeld-dir-a "/gone"))
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (d) (equal d "/b"))))
      (with-temp-buffer
        (setq default-directory "/b/")
        (should-error (emeld-compare-marked-dirs) :type 'user-error)))))

(ert-deftest emeld-list-operations-shows-log ()
  "`emeld-list-operations' renders one row per logged entry."
  (let ((src (generate-new-buffer " *emeld-ops-src*")))
    (unwind-protect
        (progn
          (with-current-buffer src
            (setq-local emeld--op-log
                        (list (list :type 'delete :path "/l/gone.txt"
                                    :trash "/trash/gone.txt"
                                    :time (current-time) :is-dir nil)
                              (list :type 'copy :src "/l/a.txt" :dest "/r/a.txt"
                                    :time (current-time) :is-dir nil
                                    :mirror nil)))
            (emeld-list-operations))
          (with-current-buffer "*emeld-operations*"
            (should (eq major-mode 'emeld-operations-mode))
            (goto-char (point-min))
            (search-forward "gone.txt")
            (goto-char (point-min))
            (search-forward "a.txt"))
          (kill-buffer "*emeld-operations*"))
      (when (buffer-live-p src) (kill-buffer src)))))

(ert-deftest emeld-operations-undo-from-point ()
  "Undoing a viewer row reverses that entry and drains it from the log."
  (let* ((tmp (make-temp-file "emeld-ops" t))
         (trash-directory (expand-file-name "TRASH" tmp))
         (dest (expand-file-name "copied.txt" tmp))
         (src (generate-new-buffer " *emeld-ops-src*")))
    (unwind-protect
        (progn
          (with-temp-file dest (insert "copied"))
          (with-current-buffer src
            (setq-local emeld--left-root tmp
                        emeld--right-root tmp
                        emeld--op-log (list (list :type 'copy :src "/elsewhere/a.txt"
                                                 :dest dest :time (current-time)
                                                 :is-dir nil :mirror nil)))
            (emeld-list-operations))
          (with-current-buffer "*emeld-operations*"
            (goto-char (point-min))
            (cl-letf (((symbol-function 'emeld-refresh) (lambda () nil)))
              (emeld-operations-undo)))
          (should-not (file-exists-p dest))
          (should (null (buffer-local-value 'emeld--op-log src)))
          (kill-buffer "*emeld-operations*"))
      (when (buffer-live-p src) (kill-buffer src))
      (delete-directory tmp t))))

(ert-deftest emeld-copy-builtin-async-logs ()
  "Built-in copies run node-at-a-time and push undoable log entries."
  (let* ((left (make-temp-file "emeldL" t))
         (right (make-temp-file "emeldR" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.txt" left) (insert "x"))
          (with-temp-file (expand-file-name "b.txt" left) (insert "y"))
          (with-temp-buffer
            (setq-local emeld--left-root left
                        emeld--right-root right
                        emeld--marked-paths (make-hash-table :test #'equal)
                        emeld--op-log nil)
            (cl-letf (((symbol-function 'emeld-refresh) (lambda () nil)))
              (emeld--copy-builtin-async
               (list (cons (expand-file-name "a.txt" left) right)
                     (cons (expand-file-name "b.txt" left) right))
               '("a.txt" "b.txt"))
              ;; The chain yields between nodes: it stays busy until the
              ;; pending timers have all been run.
              (should (emeld--copy-busy-p))
              (while (timerp emeld--copy-timer)
                (timer-event-handler emeld--copy-timer))
              (should-not (emeld--copy-busy-p))
              (should (file-exists-p (expand-file-name "a.txt" right)))
              (should (file-exists-p (expand-file-name "b.txt" right)))
              (should (= 2 (length emeld--op-log)))
              (should (eq 'copy (plist-get (car emeld--op-log) :type)))
              ;; Newest entry first: b.txt was copied last.
              (should (equal (expand-file-name "b.txt" right)
                             (plist-get (car emeld--op-log) :dest))))))
      (delete-directory left t)
      (delete-directory right t))))

(ert-deftest emeld-copy-method-effective ()
  "The reported method follows `emeld-copy-method' and rsync availability."
  (let ((emeld-copy-method 'auto))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) "/usr/bin/rsync")))
      (should (eq 'rsync (emeld--copy-method-effective))))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
      (should (eq 'builtin (emeld--copy-method-effective)))))
  (let ((emeld-copy-method 'builtin))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) "/usr/bin/rsync")))
      (should (eq 'builtin (emeld--copy-method-effective))))))

(ert-deftest emeld-stop-scan-cancels-copy-timer ()
  "A rescan or buffer kill aborts a pending built-in copy chain."
  (with-temp-buffer
    (setq emeld--copy-timer (run-with-timer 100 nil #'ignore))
    (emeld--stop-scan)
    (should-not emeld--copy-timer)))

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

(ert-deftest emeld-expanding-unscanned-dir-populates-subtree ()
  "Expanding a dir left unscanned by a depth-limited scan enumerates it.
The subtree is diffed in place, and ancestors stop being flagged for
drilling once nothing below them remains unscanned."
  (let* ((left (make-temp-file "emeld-l" t))
         (right (make-temp-file "emeld-r" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "sub/d" left) t)
          (make-directory (expand-file-name "sub/d" right) t)
          ;; d/ holds one differing and one identical file.
          (with-temp-file (expand-file-name "sub/d/a.txt" left) (insert "a"))
          (with-temp-file (expand-file-name "sub/d/a.txt" right) (insert "b"))
          (with-temp-file (expand-file-name "sub/d/same.txt" left) (insert "x"))
          (with-temp-file (expand-file-name "sub/d/same.txt" right) (insert "x"))
          (with-temp-buffer
            (setq-local emeld--left-root (file-name-as-directory left)
                        emeld--right-root (file-name-as-directory right)
                        emeld-respect-gitignore nil
                        emeld--active-filter-groups nil
                        emeld-show-same nil)
            ;; Mimic a depth-limited scan: sub is marked `unscanned', d below
            ;; it was never enumerated (no children, dim marker).
            (let* ((root (emeld-node-create :name "root" :is-dir t))
                   (sub (emeld-node-create
                         :name "sub" :is-dir t :parent root :expanded t
                         :diff-status 'unscanned :scanned nil
                         :left-path (expand-file-name "sub" left)
                         :right-path (expand-file-name "sub" right)))
                   (d (emeld-node-create
                       :name "d" :is-dir t :parent sub :expanded nil
                       :diff-status 'unscanned :scanned nil
                       :left-path (expand-file-name "sub/d" left)
                       :right-path (expand-file-name "sub/d" right))))
              (setf (emeld-node-children sub) (list d)
                    (emeld-node-children root) (list sub)
                    emeld--root-node root)
              ;; Put point on a line tagged with the d node, and no-op the render.
              (insert (propertize "d" 'emeld-node d))
              (goto-char (point-min))
              (cl-letf (((symbol-function 'emeld--render) (lambda () nil)))
                (emeld-cycle-subtree))
              ;; The subtree is enumerated: the differing file is a child,
              ;; the identical one is not (show-same off), and both d and the
              ;; ancestor sub are scanned again.
              (let ((names (mapcar #'emeld-node-name (emeld-node-children d))))
                (should (member "a.txt" names))
                (should-not (member "same.txt" names))
                (should (eq 'different (emeld-node-diff-status d)))
                (should (emeld-node-scanned d))
                (should (emeld-node-expanded d))
                (should (emeld-node-scanned sub))
                (should (eq 'different (emeld-node-diff-status sub)))))))
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

(ert-deftest emeld-inline-diff-faceify-faces-lines ()
  "`emeld--inline-diff-faceify' tags each diff line with the right `diff-*' face."
  (let* ((text "diff --git a/f b/f\n@@ -1,2 +1,2 @@\n context\n-old\n+new\n")
         (lines (emeld--inline-diff-faceify text)))
    (should (= (length lines) 5))
    (should (eq (get-text-property 0 'face (nth 0 lines)) 'diff-header))
    (should (eq (get-text-property 0 'face (nth 1 lines)) 'diff-hunk-header))
    (should (eq (get-text-property 0 'face (nth 2 lines)) 'diff-context))
    (should (eq (get-text-property 0 'face (nth 3 lines)) 'diff-removed))
    (should (eq (get-text-property 0 'face (nth 4 lines)) 'diff-added))))

(ert-deftest emeld-inline-diff-refine-highlights-changed-words ()
  "`emeld--inline-diff-refine' marks only the changed words of a pair."
  (let* ((text (concat "@@ -1 +1 @@\n"
                       "-the quick brown fox\n"
                       "+the quick brown cat\n"
                       "@@ -3,1 +3,0 @@\n"
                       "-pure deletion\n"
                       "@@ -5,0 +6,1 @@\n"
                       "+pure addition\n"))
         (lines (emeld--inline-diff-refine (emeld--inline-diff-faceify text))))
    (should (= (length lines) 7))
    (should (eq (get-text-property 0 'face (nth 0 lines)) 'diff-hunk-header))
    ;; One spot: the removed/added runs of lines 1–2.
    (should (equal (emeld--inline-diff-spots (emeld--inline-diff-faceify text))
                   '((1 2 2 3))))
    ;; Only "fox"/"cat" changed: the shared text keeps the base face.
    (should (eq (get-text-property 4 'face (nth 1 lines)) 'diff-removed))
    (should (equal (get-text-property 17 'face (nth 1 lines))
                   '(diff-refine-removed diff-removed)))
    (should (eq (get-text-property 4 'face (nth 2 lines)) 'diff-added))
    (should (equal (get-text-property 17 'face (nth 2 lines))
                   '(diff-refine-added diff-added)))
    ;; Runs without a removed/added counterpart stay plain, since the
    ;; base face already marks them in full.
    (should (cl-loop for pos below (length (nth 4 lines))
                     always (eq (get-text-property pos 'face (nth 4 lines))
                                'diff-removed)))
    (should (cl-loop for pos below (length (nth 6 lines))
                     always (eq (get-text-property pos 'face (nth 6 lines))
                                'diff-added)))))

(ert-deftest emeld-inline-diff-lines-dir-mode ()
  "Inline preview yields diff lines for a differing file and nil for identical."
  (let* ((left (make-temp-file "emeld-l" t))
         (right (make-temp-file "emeld-r" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "f.txt" left) (insert "old\n"))
          (with-temp-file (expand-file-name "f.txt" right) (insert "new\n"))
          (with-temp-file (expand-file-name "same.txt" left) (insert "x\n"))
          (with-temp-file (expand-file-name "same.txt" right) (insert "x\n"))
          (with-temp-buffer
            (setq-local emeld--git-mode nil)
            (let* ((root (emeld-node-create :name "" :is-dir t :expanded t))
                   (diff (emeld-node-create
                          :name "f.txt"
                          :left-path (expand-file-name "f.txt" left)
                          :right-path (expand-file-name "f.txt" right)
                          :diff-status 'different :parent root))
                   (same (emeld-node-create
                          :name "same.txt"
                          :left-path (expand-file-name "same.txt" left)
                          :right-path (expand-file-name "same.txt" right)
                          :diff-status 'same :parent root)))
              (let ((lines (emeld--inline-diff-lines diff)))
                (should lines)
                (should (cl-some (lambda (l) (string-match-p "new" l)) lines))
                (should (cl-some (lambda (l) (string-match-p "old" l)) lines)))
              ;; Changed words are refined by default …
              (let* ((lines (emeld--inline-diff-lines diff))
                     (added (cl-find-if #'emeld--inline-diff-added-line-p lines)))
                (should (equal (get-text-property (string-match "old" added)
                                                  'face added)
                               '(diff-refine-added diff-added))))
              ;; … but not when the peek has more spots than allowed, where
              ;; the plain `diff-*' faces are used alone.
              (let* ((emeld-inline-diff-refine-max-spots 0)
                     (lines (emeld--inline-diff-lines diff))
                     (added (cl-find-if #'emeld--inline-diff-added-line-p lines)))
                (should (eq (get-text-property (string-match "old" added)
                                               'face added)
                            'diff-added)))
              ;; An identical file has nothing to preview.
              (should-not (emeld--inline-diff-lines same)))))
      (delete-directory left t)
      (delete-directory right t))))

(ert-deftest emeld-git-history-working-tree-slot ()
  "Index -1 is the working-tree slot: newest commit on the left, working right."
  (with-temp-buffer
    (setq emeld--git-mode t
          emeld--git-repo "/tmp/repo/"
          emeld--git-history '("aaa" "bbb" "ccc")
          emeld--root-node (emeld-node-create :name "" :name-right "" :is-dir t))
    (cl-letf (((symbol-function 'emeld-refresh) #'ignore))
      (setq emeld--git-history-index -1)
      (emeld--git-history-apply)
      (should (eq emeld--git-right-rev 'working))
      (should (equal emeld--git-left-rev "aaa"))
      (should (= emeld--git-history-index -1)))))

(ert-deftest emeld-git-rev-label-head ()
  "A SHA that equals the cached HEAD is labelled HEAD (SHA)."
  (with-temp-buffer
    (setq emeld--git-head-sha "0123456789abcdef0123456789abcdef01234567")
    (should (equal (emeld--git-rev-label emeld--git-head-sha)
                   "HEAD (01234567)"))
    ;; A different SHA is still just abbreviated.
    (should (equal (emeld--git-rev-label "fedcba9876543210fedcba9876543210fedcba98")
                   "fedcba98"))))

(ert-deftest emeld-sidebar-build-and-visible ()
  "The sidebar builds a lazy tree, hides dotfiles, sorts dirs before files."
  (let ((dir (make-temp-file "emeld-sb" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "sub" dir))
          (with-temp-file (expand-file-name "b.txt" dir) (insert "b"))
          (with-temp-file (expand-file-name "a.txt" dir) (insert "a"))
          (with-temp-file (expand-file-name ".hidden" dir) (insert "x"))
          (with-temp-file (expand-file-name "sub/deep.txt" dir) (insert "d"))
          (with-temp-buffer
            (setq-local emeld--sidebar-root (file-name-as-directory dir)
                        emeld-sidebar-show-hidden nil)
            (emeld--sidebar-build)
            (let ((kids (emeld-node-children emeld--root-node)))
              ;; dir first, then files alphabetically; .hidden excluded
              (should (equal (mapcar #'emeld-node-name kids)
                             '("sub" "a.txt" "b.txt")))
              ;; lazy: the subdir is not yet populated
              (should-not (emeld-node-scanned (car kids)))
              (emeld--sidebar-populate (car kids))
              (should (emeld-node-scanned (car kids)))
              (should (equal (mapcar #'emeld-node-name
                                     (emeld-node-children (car kids)))
                             '("deep.txt"))))
            ;; show-hidden reveals the dotfile
            (setq-local emeld-sidebar-show-hidden t)
            (emeld--sidebar-build)
            (should (member ".hidden"
                            (mapcar #'emeld-node-name
                                    (emeld-node-children emeld--root-node))))))
      (delete-directory dir t))))

(ert-deftest emeld-sidebar-reveal-expands-ancestors ()
  "Revealing a nested file expands its ancestor dirs and lands point on it."
  (let ((dir (make-temp-file "emeld-sb" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "a/b" dir) t)
          (with-temp-file (expand-file-name "a/b/f.txt" dir) (insert "x"))
          (with-temp-buffer
            (emeld-sidebar-mode)
            (setq emeld--sidebar-root (file-name-as-directory dir))
            (emeld--sidebar-build)
            (let ((f (expand-file-name "a/b/f.txt" dir)))
              (emeld--sidebar-reveal f)
              (should (equal emeld--sidebar-current f))
              ;; ancestor dirs are expanded
              (let* ((a (cl-find "a" (emeld-node-children emeld--root-node)
                                 :key #'emeld-node-name :test #'string=))
                     (b (cl-find "b" (emeld-node-children a)
                                 :key #'emeld-node-name :test #'string=)))
                (should (emeld-node-expanded a))
                (should (emeld-node-expanded b)))
              ;; point sits on the revealed file
              (should (equal (emeld--sidebar-path-at-point) f))
              ;; and the current-file highlight overlay is on its line
              (should (overlayp emeld--sidebar-hl-overlay))
              (should (eq (overlay-get emeld--sidebar-hl-overlay 'face)
                          'emeld-sidebar-highlight-face))
              (save-excursion
                (goto-char (overlay-start emeld--sidebar-hl-overlay))
                (should (equal (emeld--sidebar-path-at-point) f))))))
      (delete-directory dir t))))

(ert-deftest emeld-sidebar-xref-candidate-file ()
  "The xref candidate at point resolves to its file; non-item lines yield nil."
  (let ((dir (make-temp-file "emeld-xr" t)))
    (unwind-protect
        (let* ((f1 (expand-file-name "one.el" dir))
               (f2 (expand-file-name "two.el" dir))
               (xbuf (get-buffer-create "*emeld-xref-cand-test*")))
          (with-temp-file f1 (insert "(defun one () 1)"))
          (with-temp-file f2 (insert "(defun two () 2)"))
          (require 'xref)
          (with-current-buffer xbuf
            (xref--xref-buffer-mode)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (propertize "one: 1\n" 'xref-item
                                  (xref-make "one"
                                             (xref-make-file-location f1 1 0)))
                      (propertize "=== group header ===\n")
                      (propertize "two: 2\n" 'xref-item
                                  (xref-make "two"
                                             (xref-make-file-location f2 1 0)))))
            (goto-char (point-min))
            (search-forward "two")
            (should (equal (emeld--sidebar-xref-candidate-file) f2))
            ;; a line without an xref item answers nil
            (goto-char (point-min))
            (search-forward "group header")
            (should-not (emeld--sidebar-xref-candidate-file)))
          (kill-buffer xbuf)
          (let ((buf (get-file-buffer f1))) (when buf (kill-buffer buf)))
          (let ((buf (get-file-buffer f2))) (when buf (kill-buffer buf))))
      (delete-directory dir t))))

(ert-deftest emeld-sidebar-follow-xref-wiring ()
  "Follow mode installs the preview follower on xref buffers and removes it."
  (let ((xbuf (get-buffer-create "*emeld-xref-wiring-test*")))
    (unwind-protect
        (progn
          (require 'xref)
          (with-current-buffer xbuf (xref--xref-buffer-mode))
          ;; before: no follower
          (with-current-buffer xbuf
            (should-not (memq #'emeld--sidebar-follow-xref post-command-hook)))
          ;; enabling follow mode retro-fits existing xref buffers
          (emeld-sidebar-follow-mode 1)
          (with-current-buffer xbuf
            (should (memq #'emeld--sidebar-follow-xref post-command-hook)))
          ;; disabling scrubs them again
          (emeld-sidebar-follow-mode -1)
          (with-current-buffer xbuf
            (should-not (memq #'emeld--sidebar-follow-xref post-command-hook)))
          (should-not (memq #'emeld--sidebar-follow window-selection-change-functions))
          (should-not (memq #'emeld--sidebar-follow window-buffer-change-functions))
          ;; future xref buffers pick the follower up from the mode hook
          (emeld-sidebar-follow-mode 1)
          (should (memq #'emeld--sidebar-xref-mode-setup xref--xref-buffer-mode-hook))
          (emeld-sidebar-follow-mode -1)
          (should-not (memq #'emeld--sidebar-xref-mode-setup
                            xref--xref-buffer-mode-hook)))
      (kill-buffer xbuf))))

(ert-deftest emeld-sidebar-follow-changed-window ()
  "Follow reveals a file shown in a changed NON-selected window.
This is the xref candidate-preview situation: the selected window shows
a non-file buffer (the `*xref*' list, or the minibuffer during
completing-read) while another window temporarily displays each
candidate's file."
  (let ((dir (make-temp-file "emeld-fw" t)))
    (unwind-protect
        (let* ((f1 (expand-file-name "a.el" dir))
               (f2 (expand-file-name "b/deep.el" dir))
               (sb-buf (get-buffer-create emeld--sidebar-buffer-name))
               buf1 buf2 sb-win)
          (make-directory (expand-file-name "b" dir))
          (with-temp-file f1 (insert "(defun aa () 1)"))
          (with-temp-file f2 (insert "(defun bb () 1)"))
          (setq buf1 (find-file-noselect f1)
                buf2 (find-file-noselect f2))
          (with-current-buffer sb-buf
            (emeld-sidebar-mode)
            (setq emeld--sidebar-root (file-name-as-directory dir))
            (emeld--sidebar-build))
          (setq sb-win (split-window))
          (set-window-buffer sb-win sb-buf)
          (clrhash emeld--sidebar-follow-seen)
          (emeld-sidebar-follow-mode 1)
          (unwind-protect
              (progn
                ;; normal case: the selected window shows f1
                (set-window-buffer (selected-window) buf1)
                (emeld--sidebar-follow)
                (should (equal (buffer-local-value
                                'emeld--sidebar-current sb-buf)
                               f1))
                ;; preview case: the selected window becomes a non-file
                ;; buffer while the other window switches to the candidate
                (let ((main (selected-window))
                      (other (split-window)))
                  (set-window-buffer main (get-buffer-create "*emeld-fake-xref*"))
                  (set-window-buffer other buf1)
                  (emeld--sidebar-follow)      ; prime the window->buffer map
                  (set-window-buffer other buf2) ; preview shows candidate f2
                  (emeld--sidebar-follow)      ; window-buffer-change fires
                  (should (equal (buffer-local-value
                                  'emeld--sidebar-current sb-buf)
                                 f2))))
            (emeld-sidebar-follow-mode -1)
            (delete-other-windows)
            (clrhash emeld--sidebar-follow-seen)
            (kill-buffer buf1)
            (kill-buffer buf2)
            (kill-buffer sb-buf)))
      (delete-directory dir t))))

(message "Running tests...")
(ert-run-tests-batch-and-exit)
