;;; emeld.el --- Meld-Inspired Local Diff -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Maintainer: James Dyer <captainflasmr@gmail.com>
;; Keywords: files, tools
;; Version: 0.46.0
;; Package-Version: 0.46.0
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))
;; URL: https://github.com/captainflasmr/emeld

;;; Commentary:
;; A side-by-side directory comparison tool inspired by Meld.
;; `emeld-diff' compares two directories; `emeld-git-diff' compares two
;; git revisions (or a revision against the working tree / index); and
;; `emeld-sidebar' is a treemacs-style file-tree explorer.
;;
;; This file is a thin umbrella: it loads the shared core and the feature
;; modules so that `(require 'emeld)' pulls in everything.
;;
;;   emeld-core.el    -- struct, scan pipeline, render, mode, faces, filters
;;   emeld-git.el     -- git revision comparison and history stepping
;;   emeld-sidebar.el -- treemacs-style file-tree sidebar

;;; Code:

(require 'emeld-core)
(require 'emeld-git)
(require 'emeld-sidebar)

(provide 'emeld)
;;; emeld.el ends here
