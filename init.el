;;; init.el --- Emacs Init File

;;; Commentary:
;;; My Emacs Init File
;;; I first declare the scripts folder in which I store my personal scripts for Emacs
;;; Then the elisp folders contains package declaration/config and editor config

;;; Code:

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

;; Custom params in another file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Personal scripts
(add-to-list 'load-path "~/.emacs.d/scripts/theme-switcher")
(add-to-list 'load-path "~/.emacs.d/scripts/new-term")

;; Add init config to load-path
(add-to-list 'load-path "~/.emacs.d/elisp")

;; Require scripts/
(require 'theme-switcher)
(require 'new-term)

;; Require config in elisp/
(require 'themes)
(require 'global)
(require 'keybindings)
(require 'buffer)
(require 'git)
(require 'ivy-counsel)
(require 'org-mode)
(require 'lsp-init)

(provide 'init)

;;; init.el ends here
