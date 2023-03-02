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

;; Linting
(use-package flycheck
  :config
  (global-flycheck-mode))


;; Git

;; Magit
(use-package magit
  :defer t
  :bind ("C-x g" . 'magit-status))

;; git gutter to track changes
(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  (setq git-gutter:hide-gutter t))


;; Org mode

(use-package org
  :defer t
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
  (setq org-agenda-files '("~/Notes/org-roam"))
  (setq org-todo-keywords
        '((sequence "TODO" "ONGOING" "|" "DONE" "WONTDO"))))

(use-package org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-directory (file-truename "~/Notes/org-roam"))
  (org-roam-db-autosync-mode)
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}
#+STARTUP: overview")
           :unnarrowed t)
          ("s" "sprint" plain "%?"
           :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}
#+TODO: TO_DO(o) IN_PROGRESS(p) TO_REVIEW(r) TO_TEST(t) READY_TO_MERGE(m) READY_TO_DEPLOY(d) | DONE(D)
#+STARTUP: overview")
           :unnarrowed t))))

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


;; Project wide commands
(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-register-project-type 'rails-rspec '("Gemfile" "app" "lib" "db" "config" "spec")
                                    :project-file "Gemfile"
                                    :compile "start-rails-server"
                                    :src-dir "app/"
                                    :test "launch-test"
                                    :test-dir "spec/unit/"
                                    :test-suffix "_spec"))


;; Vterm integration
(use-package vterm
  :load-path  "~/.emacs.d/emacs-libvterm/")


;; Snippets
(use-package yasnippet
  :config (yas-global-mode t))

(provide 'tools)
;;; tools.el ends here
