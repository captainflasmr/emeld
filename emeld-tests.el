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

(message "Running tests...")
(ert-run-tests-batch-and-exit)
