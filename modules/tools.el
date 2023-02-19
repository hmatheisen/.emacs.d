;;; tools.el --- Some third party tools I use        -*- lexical-binding: t; -*-

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

;; Most of these shoud live somewhere else in a dedicated file

;;; Code:



;; Git goodness
(use-package magit
  :defer t
  :bind ("C-x g" . 'magit-status))

;; git gutter to track changes
(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t)
  (setq git-gutter:hide-gutter t))

;; Org mode goodness
(use-package org
  :defer t
  :diminish visual-line-mode auto-fill-function
  :preface
  (defun he/org-mode-hook ()
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

  :hook ((org-mode . he/org-mode-hook)
         (org-indent-mode . (lambda () (diminish 'org-indent-mode)))
         (flyspell-mode . (lambda () (diminish 'flyspell-mode)))
         (buffer-face-mode . (lambda () (diminish 'buffer-face-mode))))

  :config
  ;; Recommended bindings
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  ;; Unbind C-<tab> to use 'tab-next
  (define-key org-mode-map (kbd "C-<tab>") nil)
  (setq org-todo-keywords
        '((sequence "TODO" "ONGOING" "|" "DONE" "WONTDO")
          (sequence "TO_DO" "IN_PROGRESS" "TO_REVIEW" "TO_TEST" "READY_TO_MERGE" "|" "READY_TO_DEPLOY"))))

(use-package toc-org
  :defer t
  :hook ((org-mode      . toc-org-mode)
         (markdown-mode . toc-org-mode)))

;; Quick search
(use-package ripgrep
  :config
  (global-set-key (kbd "C-c s") #'rg-menu)
  ;; Quick regexp search in project
  (rg-define-search rg-search-all
    :format regexp
    :files "everything"
    :dir project)
  (global-set-key (kbd "C-c C-s") #'rg-search-all))

;; project commands
(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; Vterm integration
(use-package vterm
  :load-path  "~/.emacs.d/emacs-libvterm/")

;; Snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode t))

(provide 'tools)
;;; tools.el ends here
