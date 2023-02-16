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
    (set-face-attribute 'org-document-title nil :height 2.0)
    (custom-set-faces
     '(org-level-1 ((t (:inherit outline-1 :height 1.8))))
     '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
     '(org-level-3 ((t (:inherit outline-3 :height 1.4))))
     '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
     '(org-level-5 ((t (:inherit outline-5 :height 1.0))))))
  :hook ((org-mode . he/org-mode-hook)
         (org-indent-mode . (lambda ()
                              (diminish 'org-indent-mode)))
         (flyspell-mode . (lambda ()
                            (diminish 'flyspell-mode)))
         (buffer-face-mode . (lambda ()
                               (diminish 'buffer-face-mode))))
  :config
  ;; Unbind C-<tab> to use 'tab-next
  (define-key org-mode-map (kbd "C-<tab>") nil)
  (setq org-todo-keywords
        '((sequence "TODO" "ONGOING" "|" "DONE" "WONTDO"))))

(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode))

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

