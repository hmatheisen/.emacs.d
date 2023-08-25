;;; nry-tools.el --- Very useful third party tools   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Henry MATHEISEN

;; Author: Henry MATHEISEN <haineriz@posteo.de>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:

;; Magit
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . 'magit-status)
          :map magit-file-section-map
          ("RET" . magit-diff-visit-file-other-window)
          :map magit-hunk-section-map
          ("RET" . magit-diff-visit-file-other-window))
  :config
  (transient-append-suffix 'magit-log "-A"
    '("-m" "No Merges" "--no-merges")))

;; git gutter to track changes
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t)
  (setq git-gutter:hide-gutter t))

(provide 'nry-tools)
;;; nry-tools.el ends here
