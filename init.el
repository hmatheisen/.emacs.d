;;; init.el --- Emacs Init File

;;; Commentary:
;;; My Emacs Init File
;;; I first declare the scripts folder in which I store my personal scripts for Emacs
;;; Then the elisp folders contains package declaration/config and editor config

;;; Code:

;; MELPA
(package-initialize)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(package-selected-packages
   (quote
    (htmlize yasnippet-snippets 2048-game undo-tree flymd auctex latex-preview-pane tern web-mode flymake-eslint yaml-mode nginx-mode dockerfile-mode docker all-the-icons tide company-tern tern-auto-complete company-lsp lsp-ui lsp-mode flycheck projectile cider clojure-mode neotree spacemacs-theme dap-mode elpy js2-mode org-bullets org markdown-mode magit go-mode company counsel ivy move-text buffer-move auto-package-update use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Require my own scripts
(add-to-list 'load-path "~/.emacs.d/scripts/new-term")
(add-to-list 'load-path "~/.emacs.d/scripts/theme-switcher")

;; Require other config files
(add-to-list 'load-path "~/.emacs.d/elisp")

(load "~/.emacs.d/elisp/packages")
(load "~/.emacs.d/elisp/global")
(load "~/.emacs.d/elisp/buffer")
(load "~/.emacs.d/elisp/keybindings")
(load "~/.emacs.d/elisp/ivy-mode")
(load "~/.emacs.d/elisp/company-mode")
(load "~/.emacs.d/elisp/golang")
(load "~/.emacs.d/elisp/git")
(load "~/.emacs.d/elisp/markdown")
(load "~/.emacs.d/elisp/org-mode")
(load "~/.emacs.d/elisp/javascript")
(load "~/.emacs.d/elisp/python-mode")
(load "~/.emacs.d/elisp/lsp-conf")
(load "~/.emacs.d/elisp/web.el")

;;; init.el ends here
(put 'set-goal-column 'disabled nil)
