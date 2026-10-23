;;; emeld-example.el --- Generate emeld's demo comparison trees -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Dyer

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Builds the two-directory comparison used by the manual walkthroughs
;; (and the video recordings) and by the fixture-driven tests in
;; `emeld-tests.el'.  It covers every user-visible feature:
;;
;;   identical files (same), modified (different), left-only (new),
;;   right-only (deleted), empty files, binary files, CRLF-vs-LF files,
;;   files without a trailing newline, whitespace/blank-line/case/comment-only
;;   changes, files with several hunks and with duplicate blocks,
;;   names with spaces and non-ASCII characters, a symbolic link,
;;   deep nesting, a directory of many files, filter-group and gitignore
;;   candidates, session-hide targets, and a directory tree to demo the
;;   chunked built-in copy.
;;
;; It also builds a small git repository (`emeld-example-make-git') with
;; four commits, a branch, and staged/unstaged working-tree changes for the
;; git-mode walkthroughs.
;;
;; Usage (from the repository root):
;;
;;   emacs -Q --batch -L . -l emeld.el -l test/emeld-example.el   \
;;     --eval '(emeld-example-make "test/example")'
;;   emacs -Q --batch -L . -l emeld.el -l test/emeld-example.el   \
;;     --eval '(emeld-example-make-git "test/example/git-repo")'
;;
;; or simply run `test/make-example.sh'.  The generated directory is
;; gitignored: the generator, not the bytes, is the source of truth.
;; Generation is deterministic (fixed contents and modification times), so
;; recordings and tests are reproducible.
;;
;; `emeld-example-make' returns a manifest — an alist of (RELPATH . STATUS)
;; for every file the generator made differ — which the tests compare
;; against a real scan.

;;; Code:

(require 'emeld)

(defconst emeld-example-mtime (encode-time 0 4 3 2 1 2026)
  "Fixed modification time (2026-01-02 03:04) given to every example file.")

(defvar emeld-example--left nil
  "Absolute left root of the example comparison being generated.")
(defvar emeld-example--right nil
  "Absolute right root of the example comparison being generated.")
(defvar emeld-example--manifest nil
  "Manifest being built by `emeld-example-make' (newest entry first).")

(defun emeld-example--write (path content &optional binary)
  "Write CONTENT to PATH, creating parent directories.
When BINARY is non-nil, CONTENT's characters are written as raw bytes
\(used for CRLF and binary demo files); otherwise it is written as UTF-8."
  (make-directory (file-name-directory path) t)
  (with-temp-buffer
    (set-buffer-multibyte (not binary))
    (insert content)
    (let ((coding-system-for-write (if binary 'no-conversion 'utf-8-unix)))
      (write-region (point-min) (point-max) path nil 'silent))))

(defun emeld-example--file (rel left right &optional binary)
  "Create REL from LEFT/RIGHT — strings, or nil when absent on that side.
BINARY writes raw bytes.  Records REL's expected scan status in
`emeld-example--manifest' whenever the two sides differ."
  (let ((left-path (expand-file-name rel emeld-example--left))
        (right-path (expand-file-name rel emeld-example--right)))
    (when left (emeld-example--write left-path left binary))
    (when right (emeld-example--write right-path right binary))
    (unless (equal left right)
      (push (cons rel (cond ((null left) 'right-only)
                            ((null right) 'left-only)
                            (t 'different)))
            emeld-example--manifest))))

(defun emeld-example--binary-content (tail)
  "Return a unibyte string of bytes 0-255 followed by the string TAIL."
  (apply #'unibyte-string
         (append (number-sequence 0 255) (string-to-list tail))))

(defun emeld-example--link (rel target)
  "Create REL as a symbolic link to relative TARGET on both sides."
  (dolist (root (list emeld-example--left emeld-example--right))
    (let ((path (expand-file-name rel root)))
      (make-directory (file-name-directory path) t)
      (when (file-exists-p path) (delete-file path))
      (make-symbolic-link target path t))))

(defun emeld-example--touch-tree (root)
  "Give every file and directory under ROOT the fixed example mtime."
  (dolist (path (directory-files-recursively root "" t))
    (set-file-times path emeld-example-mtime)))

(defun emeld-example--numbers (prefix n &optional suffix)
  "Return N lines named PREFIX000..PREFIX(N-1) with optional SUFFIX."
  (mapconcat (lambda (i)
               (format "%s%03d%s\n" prefix (1+ i) (or suffix "")))
             (number-sequence 1 n)
             ""))

(defun emeld-example-make (dir)
  "Generate the example comparison under DIR and return its manifest.
DIR gets `left/' and `right/' trees; the manifest is an alist of
\(RELPATH . STATUS) covering every file the generator made differ, in
creation order.  The roots get identical `.dir-locals.el' files holding
the default filter groups (so nothing is created on the first scan).
Requires emeld to be loaded."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (emeld-example--left (expand-file-name "left/" dir))
         (emeld-example--right (expand-file-name "right/" dir))
         (emeld-example--manifest nil))
    (when (file-exists-p dir) (delete-directory dir t))
    (make-directory dir t)
    (make-directory emeld-example--left t)
    (make-directory emeld-example--right t)

    ;; A throwaway work tree at DIR keeps the fixture's own .gitignore files
    ;; authoritative when the fixture lives inside another repository: git
    ;; work-tree discovery stops at DIR/.git, so the enclosing repository's
    ;; ignore rules (e.g. a blanket `test/example/') cannot mark the whole
    ;; fixture ignored.  Skipped when git is unavailable.
    (when (executable-find "git")
      (emeld-example--git dir "init" "-q"))

    ;; ── Baseline: identical on both sides (shown with `S' / hide with `S')
    (emeld-example--file "README.md" "# Emeld example\n\nIdentical on both sides.\n"
                         "# Emeld example\n\nIdentical on both sides.\n")
    (emeld-example--file "same.txt" "unchanged\n" "unchanged\n")
    (emeld-example--file "empty.txt" "" "")
    (emeld-example--file "big.txt"
                         (emeld-example--numbers "line " 200)
                         (emeld-example--numbers "line " 200))
    (emeld-example--file "src/util.el"
                         ";;; util.el --- shared helpers\n\n(provide 'util)\n"
                         ";;; util.el --- shared helpers\n\n(provide 'util)\n")

    ;; ── Modified files: peek, hunk copy, ediff, patch export
    (emeld-example--file "modified.txt"
                         "The quick brown fox\njumps over the lazy dog.\n"
                         "The quick brown fox\nleaps over the lazy dog.\n")
    (let* ((lines (mapcar (lambda (i) (format "context %03d\n" (1+ i)))
                          (number-sequence 1 30)))
           (right-lines (copy-sequence lines)))
      ;; Three changes, far enough apart to form three separate hunks.
      (dolist (i '(2 14 26))
        (setcar (nthcdr i right-lines) (format "changed %03d\n" (1+ i))))
      (emeld-example--file "multi-hunk.txt"
                           (apply #'concat lines)
                           (apply #'concat right-lines)))
    (emeld-example--file "duplicate-blocks.txt"
                         "x\nfoo\ny\nx\nfoo\ny\n"
                         "x\nfoo\ny\nx\nFOO\ny\n")
    (emeld-example--file "no-newline.txt" "alpha\nbeta" "alpha\nBETA")
    (emeld-example--file "src/main.el"
                         "(defun main ()\n  (message \"left version\"))\n"
                         "(defun main ()\n  (message \"right version\"))\n")
    (emeld-example--file "src/deep/level1/level2/level3/deep-file.txt"
                         "deep on the left\n" "deep on the right\n")
    (emeld-example--file "docs/guide.md"
                         "# Guide\n\nSee the left side.\n"
                         "# Guide\n\nSee the right side.\n")
    (emeld-example--file "spaces in name.txt"
                         "left with spaces\n" "right with spaces\n")
    (emeld-example--file "unicode-é.txt" "gauche\n" "droite\n")

    ;; ── Content-filter demos (each becomes identical under its filter)
    (emeld-example--file "whitespace.txt"
                         "def f():\n    return 1\n"
                         "def f():\n  return 1\n")
    (emeld-example--file "blanklines.txt" "one\ntwo\n" "one\n\ntwo\n")
    (emeld-example--file "case.txt" "Hello World\nSecond line\n"
                         "hello world\nSECOND LINE\n")
    (emeld-example--file "comments.c"
                         "int main(void) {\n    // left note\n    return 0;\n}\n"
                         "int main(void) {\n    // right note\n    return 0;\n}\n")
    (emeld-example--file "crlf.txt" "one\ntwo\n" "one\r\ntwo\r\n" t)

    ;; ── Binary demos (differ; the peek reports "Binary files ... differ")
    (emeld-example--file "binary.bin"
                         (emeld-example--binary-content "LEFT")
                         (emeld-example--binary-content "RIGHT") t)
    (emeld-example--file "docs/images/logo.bin"
                         (emeld-example--binary-content "logo-left")
                         (emeld-example--binary-content "logo-right") t)

    ;; ── One-sided files and directories (copy/delete/marking demos)
    (emeld-example--file "new-file.txt" "brand new\n" nil)
    (emeld-example--file "deleted-file.txt" nil "was deleted\n")
    (emeld-example--file "to-copy.txt" "copy me across\n" nil)
    (emeld-example--file "to-delete.txt" nil "delete me\n")
    (emeld-example--file "left-only-dir/with-file.txt" "left only\n" nil)
    (emeld-example--file "right-only-dir/with-file.txt" nil "right only\n")

    ;; ── A tree for the chunked built-in copy (mark it, then `C')
    (dotimes (i 40)
      (emeld-example--file (format "copy-tree/sub%d/file%02d.txt" (% i 4) i)
                           (format "copy payload %d\n" i) nil))

    ;; ── Many files in one directory (lazy rendering, jump, narrow)
    (dotimes (i 40)
      (let ((rel (format "many/f%02d.txt" (1+ i)))
            (n (1+ i)))
        (if (zerop (mod n 10))
            (emeld-example--file rel (format "left %02d\n" n)
                                 (format "right %02d\n" n))
          (emeld-example--file rel (format "same %02d\n" n)
                               (format "same %02d\n" n)))))

    ;; ── Filter-group candidates (default "Backups" group hides these)
    (emeld-example--file "backups/app.bak" "left backup\n" "right backup\n")
    (emeld-example--file "backups/notes~" "left notes\n" "right notes\n")

    ;; ── gitignore candidates (hidden while `I' is on, inside a work tree)
    (emeld-example--file ".gitignore" "*.log\nbuild/\n" "*.log\nbuild/\n")
    (emeld-example--file "ignored.log" "left log\n" "right log\n")
    (emeld-example--file "build/output.txt" "left build\n" "right build\n")

    ;; ── A symlink both sides resolve identically (diff follows it)
    (emeld-example--link "symlink.txt" "same.txt")

    ;; Identical `.dir-locals.el' in both roots: the default filter groups
    ;; are already there, so a scan does not create or report one.
    (emeld--ensure-dir-locals emeld-example--left)
    (emeld--ensure-dir-locals emeld-example--right)

    (emeld-example--touch-tree emeld-example--left)
    (emeld-example--touch-tree emeld-example--right)
    (nreverse emeld-example--manifest)))

(defun emeld-example--git (dir &rest args)
  "Run git with ARGS in DIR, signalling an error when it fails."
  (let ((default-directory (file-name-as-directory dir))
        (process-environment
         (append '("GIT_AUTHOR_DATE=2026-01-02T03:04:05"
                   "GIT_COMMITTER_DATE=2026-01-02T03:04:05")
                 process-environment)))
    (with-temp-buffer
      (unless (eq 0 (apply #'call-process "git" nil t nil args))
        (error "git %s failed in %s: %s"
               (string-join args " ") dir (buffer-string))))))

(defun emeld-example-make-git (dir)
  "Create a small git repository for the git-mode walkthroughs at DIR.
Four commits touching a few files, a `demo-branch', an unstaged edit and a
staged new file, so `HEAD~1' vs the working tree shows a modification, a
deletion and an addition.  Returns DIR."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (when (file-exists-p dir) (delete-directory dir t))
    (make-directory dir t)
    (emeld-example--git dir "init" "-q")
    (emeld-example--git dir "config" "user.email" "demo@example.invalid")
    (emeld-example--git dir "config" "user.name" "Emeld Demo")
    (emeld-example--git dir "config" "commit.gpgsign" "false")

    (emeld-example--write (expand-file-name "main.txt" dir) "v1\n")
    (emeld-example--write (expand-file-name "lib/util.txt" dir) "stable\n")
    (emeld-example--write (expand-file-name "docs/readme.md" dir) "# Demo repo\n")
    (emeld-example--git dir "add" "-A")
    (emeld-example--git dir "commit" "-q" "-m" "Add initial files")

    (emeld-example--write (expand-file-name "main.txt" dir) "v2\n")
    (emeld-example--write (expand-file-name "notes.txt" dir) "temporary notes\n")
    (emeld-example--git dir "add" "-A")
    (emeld-example--git dir "commit" "-q" "-m" "Update main and add notes")

    (emeld-example--write (expand-file-name "lib/util.txt" dir) "stable v2\n")
    (emeld-example--git dir "add" "-A")
    (emeld-example--git dir "commit" "-q" "-m" "Improve util")

    (emeld-example--write (expand-file-name "main.txt" dir) "v3\n")
    (delete-file (expand-file-name "notes.txt" dir))
    (emeld-example--git dir "add" "-A")
    (emeld-example--git dir "commit" "-q" "-m" "Polish main and drop notes")

    (emeld-example--git dir "branch" "demo-branch" "HEAD~2")

    ;; Working-tree changes: an unstaged edit and a staged new file.
    (emeld-example--write (expand-file-name "main.txt" dir) "v3 + local edit\n")
    (emeld-example--write (expand-file-name "wip.txt" dir) "work in progress\n")
    (emeld-example--git dir "add" "wip.txt")
    dir))

(provide 'emeld-example)
;;; emeld-example.el ends here
