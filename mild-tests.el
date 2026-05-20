;;; mild-tests.el --- Tests for mild.el -*- lexical-binding: t; -*-

(load-file "mild.el")
(require 'ert)

(ert-deftest mild-glob-matching ()
  "Test glob to regexp conversion and matching."
  (let ((mild-filter-globs '("*.elc" "tmp/*" ".git")))
    ;; Simple glob
    (should (mild--filtered-p "test.elc" "src/test.elc"))
    (should-not (mild--filtered-p "test.el" "src/test.el"))
    
    ;; Glob with slash (relative path matching)
    (should (mild--filtered-p "foo" "tmp/foo"))
    (should-not (mild--filtered-p "tmp" "some/tmp")) ; "tmp/*" matches contents of tmp, or path containing tmp/
    
    ;; Exact match for dotfiles
    (should (mild--filtered-p ".git" ".git"))))

(ert-deftest mild-hidden-files ()
  "Test hidden file filtering."
  (let ((mild-show-hidden nil))
    (should (mild--filtered-p ".DS_Store" ".DS_Store")))
  (let ((mild-show-hidden t))
    (should-not (mild--filtered-p ".DS_Store" ".DS_Store"))))

(message "Running tests...")
(ert-run-tests-batch-and-exit)
