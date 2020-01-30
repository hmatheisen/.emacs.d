;;; init.el --- Emacs Init File

;;; Commentary:
;;; My Emacs Init File
;;; I first declare the scripts folder in which I store my personal scripts for Emacs
;;; Then the elisp folders contains package declaration/config and editor config

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" default)))
 '(package-selected-packages
   (quote
    (which-key counsel org-bullets magit spacemace-theme theme-switcher new-term use-package spacemacs-theme moe-theme flycheck buffer-move))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Check whether system is mac
(defconst *is-a-mac* (eq system-type 'darwin))

;; MELPA
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Setting up the package manager. Install if missing.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

;; Personal scripts
(add-to-list 'load-path "~/.emacs.d/scripts/theme-switcher")
(add-to-list 'load-path "~/.emacs.d/scripts/new-term")

(require 'theme-switcher)
(require 'new-term)

;; Add init config to load-path
(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'themes)
(require 'global)
(require 'keybindings)
(require 'buffer)
(require 'git)
(require 'ivy-counsel)
(require 'org-mode)

(provide 'init)

;;; init.el ends here
