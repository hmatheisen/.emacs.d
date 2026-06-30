;;; init.el --- Emacs base config file               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Henry MATHEISEN

;; Author: Henry MATHEISEN <henry.mthsn@gmail.com>
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
;; along with this program.  If not, see <https://www.gnu.icenses/>.

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-group-sort-by 'Buffer-menu-group-sort-alphabetically)
 '(Buffer-menu-use-header-line t)
 '(abbrev-suggest t)
 '(auto-insert-mode t)
 '(auto-save-default nil)
 '(backup-directory-alist '(("." . "~/.emacs.d/backups")))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(comint-history-isearch 'dwim)
 '(comint-prompt-read-only t)
 '(comment-auto-fill-only-comments t)
 '(completion-auto-select 'second-tab)
 '(completion-cycle-threshold nil)
 '(completion-eager-display 'auto)
 '(completion-eager-update t)
 '(completion-show-help nil)
 '(completions-detailed t)
 '(completions-format 'one-column)
 '(completions-group t)
 '(completions-max-height 20)
 '(completions-sort 'historical)
 '(confirm-kill-emacs 'y-or-n-p)
 '(context-menu-mode t)
 '(custom-safe-themes
   '("967c23e9ba179b80560774419f081df22e7674aac23c5c550b817e4a1ce7d058" default))
 '(default-frame-alist
   '((ns-transparent-titlebar . t) (fullscreen . maximized) (vertical-scroll-bar)
     (horizontal-scroll-bar)))
 '(delete-selection-mode t)
 '(diff-default-read-only t)
 '(diff-switches "-u -d")
 '(dired-auto-revert-buffer t)
 '(dired-do-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-listing-switches
   "-aFGhlv --dired --group-directories-first --time-style=long-iso")
 '(display-battery-mode t)
 '(display-line-numbers-width 4)
 '(dynamic-completion-mode nil)
 '(ediff-merge-split-window-function 'split-window-horizontally)
 '(ediff-quit-hook '(ediff-cleanup-mess winner-undo))
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-use-long-help-message nil)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-code-action-indications nil)
 '(eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider :inlayHintProvider))
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-idle-delay 0)
 '(electric-pair-mode t)
 '(eshell-history-append t)
 '(eshell-show-lisp-completions t)
 '(executable-prefix-env t)
 '(fill-column 80)
 '(fit-frame-to-buffer t)
 '(global-auto-revert-mode t)
 '(global-completion-preview-mode t)
 '(global-goto-address-mode t)
 '(global-reveal-mode t)
 '(global-so-long-mode t)
 '(grep-command "rg --no-heading ")
 '(grep-use-headings t)
 '(grep-use-null-device nil)
 '(help-at-pt-display-when-idle 'never nil (help-at-pt))
 '(help-clean-buttons t)
 '(help-enable-symbol-autoload t)
 '(help-enable-variable-value-editing t)
 '(help-window-keep-selected t)
 '(help-window-select t)
 '(horizontal-scroll-bar-mode nil)
 '(ibuffer-display-summary nil)
 '(ibuffer-expert t)
 '(icon-preference '(text))
 '(iconify-child-frame nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(isearch-allow-scroll t)
 '(isearch-lazy-count t)
 '(minibuffer-depth-indicate-mode t)
 '(minibuffer-electric-default-mode t)
 '(minibuffer-visible-completions t)
 '(mode-line-compact 'long)
 '(modus-themes-bold-constructs t)
 '(modus-themes-common-palette-overrides
   '((fringe unspecified) (cursor magenta-intense) (bg-region bg-cyan-intense)
     (bg-hl-line bg-blue-subtle) (fg-line-number-active fg-main)
     (fg-line-number-inactive fg-dim) (bg-line-number-active bg-hl-line)
     (bg-line-number-inactive unspecified) (bg-mode-line-active bg-lavender)
     (border-mode-line-active indigo)))
 '(modus-themes-italic-constructs t)
 '(modus-vivendi-palette-overrides '((bg-main "#181818") (fg-main "#eeeeee")))
 '(mouse-drag-and-drop-region-cross-program t)
 '(mouse-wheel-progressive-speed nil)
 '(ns-right-alternate-modifier 'none)
 '(ns-use-fullscreen-animation t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-install-upgrade-built-in nil)
 '(package-native-compile t)
 '(package-quickstart t)
 '(package-selected-packages
   '(editorconfig eglot exec-path-from-shell magit marginalia markdown-mode
                  markdown-ts-mode modus-themes orderless vterm))
 '(pixel-scroll-mode t)
 '(pixel-scroll-precision-mode t)
 '(project-buffers-viewer 'project-list-buffers-ibuffer)
 '(recentf-auto-cleanup 'never)
 '(recentf-mode t)
 '(repeat-mode t)
 '(ring-bell-function 'ignore)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position 1)
 '(scroll-step 5)
 '(search-default-mode t)
 '(search-exit-option t)
 '(server-mode t)
 '(smerge-command-prefix "C-c v")
 '(standard-indent 2)
 '(tab-width 4)
 '(tempo-interactive t)
 '(term-scroll-snap-to-bottom nil)
 '(text-scale-mode-step 1.1)
 '(tool-bar-mode nil)
 '(trash-directory "~/.Trash")
 '(treesit-font-lock-level 4)
 '(truncate-lines t)
 '(undo-limit 64000000)
 '(undo-outer-limit 128000000)
 '(undo-strong-limit 64000000)
 '(use-package-always-ensure t)
 '(use-system-tooltips nil)
 '(warning-minimum-log-level :debug)
 '(which-key-mode t)
 '(windmove-default-keybindings '([ignore] meta control))
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaspace Neon" :height 110))))
 '(error ((t (:underline nil))))
 '(fill-column-indicator ((t :height 1.0 :background nil)))
 '(fixed-pitch ((t (:inherit default))))
 '(fixed-pitch-serif ((t (:inherit default))))
 '(flymake-error ((t (:underline nil))))
 '(flymake-note ((t (:underline nil))))
 '(flymake-warning ((t (:underline nil))))
 '(info-title-1 ((t (:height 1.8))))
 '(info-title-2 ((t (:height 1.4))))
 '(info-title-3 ((t (:height 1.2))))
 '(info-title-4 ((t (:height 1.1))))
 '(org-document-title ((t (:height 1.8))))
 '(org-level-1 ((t (:height 1.4))))
 '(org-level-2 ((t (:height 1.2))))
 '(org-level-3 ((t (:height 1.1))))
 '(org-level-4 ((t (:height 1.1))))
 '(warning ((t (:underline nil)))))

;;; Packages

(use-package marginalia
  :functions marginalia-mode
  :init
  (marginalia-mode))

;;; Languages

(defun hma/typescript-ts-mode-hook ()
  "Typescript mode hook."
  (set-fill-column 100)
  (eglot-ensure))

(add-hook 'typescript-ts-mode-hook #'hma/typescript-ts-mode-hook)

(defun hma/tsx-ts-mode-hook ()
  "TSX mode hook."
  (set-fill-column 100)
  (eglot-ensure))

(add-hook 'tsx-ts-mode-hook #'hma/tsx-ts-mode-hook)

;;; UI

(setq-default ns-use-proxy-icon nil)

(load-theme 'modus-vivendi)

(defun hma/prog-mode-hook ()
  "Prog mode hook."
  (flymake-mode t)
  (hl-line-mode t)
  (auto-fill-mode t)
  (display-line-numbers-mode t)
  (display-fill-column-indicator-mode t)

  (keymap-local-set "M-n" 'flymake-goto-next-error)
  (keymap-local-set "M-p" 'flymake-goto-prev-error))

(add-hook 'prog-mode-hook #'hma/prog-mode-hook)

;; Overrides default implementation to remove minor modes because I do not know
;; how to do this differently.
(setq mode-line-modes
      (let ((recursive-edit-help-echo
             "Recursive edit, type C-M-c to get out"))
        (list (propertize "%[" 'help-echo recursive-edit-help-echo)
              '(:eval (car mode-line-modes-delimiters))
	          `(:propertize ("" mode-name)
			                help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
			                mouse-face mode-line-highlight
			                local-map ,mode-line-major-mode-keymap)
	          '("" mode-line-process)
	          (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
		                  'mouse-face 'mode-line-highlight
		                  'local-map (make-mode-line-mouse-map
				                      'mouse-2 #'mode-line-widen))
	          ;; '("" mode-line-minor-modes)
              '(:eval (cdr mode-line-modes-delimiters))
	          (propertize "%]" 'help-echo recursive-edit-help-echo)
	          " ")))

;; Keys

(keymap-global-set "M-/" 'hippie-expand)
(keymap-global-set "M-j" 'join-line)

(keymap-global-set "s-p" project-prefix-map)
(keymap-global-set "s-b" 'switch-to-buffer)
(keymap-global-set "s-r" 'recentf)
(keymap-global-set "s-t" 'treemacs)

(keymap-global-set "C-x C-b" 'ibuffer)

(keymap-global-set "s-Z" 'undo-redo)

;; In GUI mode, `C-z' hides the window and I hate it/keep accidentally hitting
;; it.
(keymap-global-set "C-z" nil)

(keymap-global-set "C-<wheel-up>" nil)
(keymap-global-set "C-<wheel-down>" nil)

(put 'help-fns-edit-variable 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'scroll-left 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;;; Window handling

;; Scroll by half page by default
(defun hma/window-half-height ()
  "Return half the height of a window."
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun hma/scroll-half-page-up-command (&optional arg)
  "Scroll up half the height of a window by default or by ARG."
  (interactive "^P")
  (scroll-up-command (or arg (hma/window-half-height))))

(defun hma/scroll-half-page-down-command (&optional arg)
  "Scroll down half the height of a window by default or by ARG."
  (interactive "^P")
  (scroll-down-command (or arg (hma/window-half-height))))

(keymap-global-set "C-v" #'hma/scroll-half-page-up-command)
(keymap-global-set "M-v" #'hma/scroll-half-page-down-command)

;;; Project

(require 'project)

(defmacro with-current-project (project &rest body)
  "Execute BODY with ROOT as the current PROJECT root."
  (declare (indent 1))
  `(let ((,project (project-current t)))
     ,@body))

(defun hma/copy-file-relative-path ()
  "Print and kill the absolute file path of the current buffer in a project."
  (unless buffer-file-name
    (error "project-absolute-file-path: Could not get buffer file name"))
  (with-current-project project
    (let* ((root (project-root project))
           (absolute-file-path (file-relative-name buffer-file-name root)))
      (kill-new absolute-file-path)
      (message (concat "Saved \"" absolute-file-path "\" to kill ring")))))

(defun project-copy-relative-path ()
  "Print and kill the absolute file path of the current buffer in a project."
  (interactive)
  (hma/copy-file-relative-path))

(keymap-set project-prefix-map "C-y" #'project-copy-relative-path)

(use-package orderless
  :ensure t
  :config
  (keymap-set minibuffer-local-completion-map "SPC" #'self-insert-command)
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t))

;;; Utils

(defun macroexpand-point (sexp)
  "Expand macro SEXP at point to temp buffer."
  (interactive (list (sexp-at-point)))
  (let ((buffer-name "*el-macroexpansion**"))
    (with-output-to-temp-buffer buffer-name
      (pp (macroexpand sexp)))
    (with-current-buffer buffer-name
      (emacs-lisp-mode)
      (view-mode))))

(defun new-buffer (new-buffer-name)
  "Create a new buffer named NEW-BUFFER-NAME and switch to it."
  (interactive "sNew buffer name: ")
  (switch-to-buffer
   (concat "*" new-buffer-name "*")))

(global-set-key (kbd "C-x B") 'new-buffer)

(defvar hma/current-time-format "%d-%m-%Y (%H:%M:%S)")

(defun hma/insert-current-date-time ()
  "Insert the current date and time into current buffer."
  (interactive)
  (insert (format-time-string hma/current-time-format (current-time))))

(use-package magit
  :functions (magit-status-setup-buffer hma/annette-magit)
  :config
  (defun hma/annette-magit ()
    "Open annette workspace."
    (interactive)
    (let ((window (selected-window))
          (second-window (split-window-right))
          (third-window (split-window-right)))
      (with-selected-window window
        (magit-status-setup-buffer "~/Code/api"))
      (with-selected-window second-window
        (magit-status-setup-buffer "~/Code/mobile-app"))
      (with-selected-window third-window
        (magit-status-setup-buffer "~/Code/carepro"))
      (balance-windows)))
  (defalias 'annette #'hma/annette-magit))

;; (add-to-list 'treesit-language-source-alist
;;              '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown"))

;;; init.el ends here
