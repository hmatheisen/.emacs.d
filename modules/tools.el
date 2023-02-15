;; Git goodness
(use-package magit
  :defer t
  :bind ("C-x g" . 'magit-status))

;; Git gutter to track changes
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
    (org-indent-mode 1)
    (visual-line-mode 1)
    (auto-fill-mode 1))
  :hook ((org-mode . he/org-mode-hook)
         (org-indent-mode . (lambda ()
                              (diminish 'org-indent-mode)))
         (flyspell-mode . (lambda ()
                            (diminish 'flyspell-mode)))
         (buffer-face-mode . (lambda ()
                               (diminish 'buffer-face-mode))))
  :config
  (set-face-attribute 'org-document-title nil :height 250)
  ;; Unbind C-<tab> to use 'next-buffer
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
  (global-set-key (kbd "C-c C-s") #'rg-search-all)
  ;; Customization
  (setq ))

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

