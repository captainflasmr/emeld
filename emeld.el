;;; emeld.el --- Meld-Inspired Local Diff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Dyer

;; Author: James Dyer <captainflasmr@gmail.com>
;; Maintainer: James Dyer <captainflasmr@gmail.com>
;; Assisted-by: OpenCode:DeepSeek V4.1 Flash
;; Keywords: files, tools
;; Version: 0.48.1
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))
;; URL: https://github.com/captainflasmr/emeld

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
;; A side-by-side directory comparison tool inspired by Meld.
;; `emeld-diff' compares two directories; `emeld-git-diff' compares two
;; git revisions (or a revision against the working tree / index);
;; `emeld-sidebar' is a treemacs-style file-tree explorer; and
;; `emeld-dired' is a tree-navigable, Dired-like file manager.
;;
;; This file is a thin umbrella: it loads the shared core and the feature
;; modules so that `(require 'emeld)' pulls in everything.
;;
;;   emeld-core.el    -- struct, scan pipeline, render, mode, faces, filters
;;   emeld-git.el     -- git revision comparison and history stepping
;;   emeld-sidebar.el -- treemacs-style file-tree sidebar
;;   emeld-dired.el   -- tree-navigable, Dired-like file manager

;;; Code:

(require 'emeld-core)
(require 'emeld-git)
(require 'emeld-sidebar)
(require 'emeld-dired)

(provide 'emeld)
;;; emeld.el ends here
