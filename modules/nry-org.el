;;; nry-org.el --- Org mode is cool                  -*- lexical-binding: t; -*-

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

;; Org mode

(use-package org
  :ensure t
  :preface
  (defun my-org-mode-hook ()
    ;; Useful writing modes
    (org-indent-mode 1)
    (visual-line-mode 1)
    (auto-fill-mode 1)
    ;; Override faces
    (set-face-attribute 'org-document-title nil :height 1.8)
    (custom-set-faces
     '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
     '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
     '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
     '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
     '(org-level-5 ((t (:inherit outline-5 :height 1.0))))))

  :hook ((org-mode . my-org-mode-hook))
  :config
  ;; Recommended bindings
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  ;; Unbind C-<tab> to use 'tab-next
  (define-key org-mode-map (kbd "C-<tab>") nil)
  (setq org-agenda-files '("~/Notes/org"))
  (setq org-todo-keywords
        '((sequence "TODO" "ONGOING" "|" "DONE" "WONTDO"))))

(use-package toc-org
  :ensure t
  :defer t
  :hook ((org-mode      . toc-org-mode)
         (markdown-mode . toc-org-mode)))

(provide 'nry-org)
;;; nry-org.el ends here
