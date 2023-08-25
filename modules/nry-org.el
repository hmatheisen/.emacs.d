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
    (windmove-mode -1))
  :hook ((org-mode . my-org-mode-hook))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c C-m" . org-modern-mode)) ;; toggle org modern
  :config
  ;; Unbind C-<tab> to use 'tab-next
  (define-key org-mode-map (kbd "C-<tab>") nil)
  ;; Files
  (setq org-directory "~/org/")
  (setq org-agenda-files (list org-directory))
  ;; Others
  (setq org-startup-folded 'fold)
  (setq org-log-done 'time)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "ONGOING(g)" "|" "DONE(d)" "WONTDO(w)")
          (sequence "TODO(T)" "IN_PROGRESS(P)" "TO_REVIEW(R)" "TO_TEST(F)" "READY_TO_MERGE(M)" "|" "DONE(D)")))
  (setq org-capture-templates
        '(("t" "Todo" entry (file (lambda () (concat org-directory "tasks.org")))
           "* TODO %?\n")
          ("i" "Idea" entry (file+headline (lambda () (concat org-directory "journal.org")) "Ideas")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("T" "Ticket" entry (file+headline (lambda () (concat org-directory "sprint.org")) "Tickets")
           "* IN_PROGRESS %?\nSCHEDULED: %T"))))

(use-package org-tempo)

(use-package org-modern
  :ensure t
  :config
  (global-org-modern-mode))

(use-package toc-org
  :ensure t
  :defer t
  :hook ((org-mode      . toc-org-mode)
         (markdown-mode . toc-org-mode)))

(provide 'nry-org)
;;; nry-org.el ends here
