;;; custom.el --- custom generated settings          -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Henry MATHEISEN

;; Author: Henry MATHEISEN <henry@macbook>
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-group-by nil)
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
 '(completion-pcm-complete-word-inserts-delimiters t)
 '(completion-show-help nil)
 '(completion-styles '(basic partial-completion substring))
 '(completions-detailed t)
 '(completions-format 'one-column)
 '(completions-group t)
 '(completions-max-height 20)
 '(completions-sort 'historical)
 '(confirm-kill-emacs 'y-or-n-p)
 '(context-menu-mode t)
 '(default-frame-alist
   '((ns-transparent-titlebar . t) (width . 120) (height . 60) (top . 100)
     (left . 100) (horizontal-scroll-bars)))
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
 '(global-completion-preview-mode nil)
 '(global-goto-address-mode t)
 '(global-reveal-mode t)
 '(global-so-long-mode t)
 '(grep-command "rg --no-heading ")
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs"
     "{arch}" "node_modules" "dist" ".yarn"))
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc"
     "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib"
     "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl"
     "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl"
     "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl"
     "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky"
     "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs"
     "*.pyc" "*.pyo" "*.log"))
 '(grep-use-headings t)
 '(grep-use-null-device nil)
 '(help-at-pt-display-when-idle 'never nil (help-at-pt))
 '(help-clean-buttons t)
 '(help-enable-symbol-autoload t)
 '(help-enable-variable-value-editing t)
 '(help-window-keep-selected t)
 '(help-window-select t)
 '(ibuffer-display-summary nil)
 '(ibuffer-expert t)
 '(icomplete-compute-delay 0)
 '(icomplete-delay-completions-threshold 0)
 '(icomplete-mode t)
 '(icon-preference '(text))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(isearch-allow-scroll t)
 '(isearch-lazy-count t)
 '(minibuffer-depth-indicate-mode t)
 '(minibuffer-electric-default-mode t)
 '(minibuffer-visible-completions t)
 '(mode-line-compact 'long)
 '(mouse-drag-and-drop-region-cross-program t)
 '(mouse-wheel-progressive-speed nil)
 '(ns-right-alternate-modifier 'none)
 '(ns-use-fullscreen-animation t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-native-compile t)
 '(package-quickstart t)
 '(package-selected-packages
   '(cmake-mode corfu csv dart-mode eldoc-box exec-path-from-shell flutter magit
                marginalia markdown-mode modus-themes multiple-cursors prettier
                rainbow-mode ruby-electric sass-mode sly treemacs vterm
                yaml-mode))
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
 '(treemacs-no-png-images t)
 '(treemacs-width 50)
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
 '(default ((t (:family "Liberation Mono" :height 140))))
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
 '(variable-pitch ((t (:height 140 :family "Roboto Flex"))))
 '(warning ((t (:underline nil)))))

(provide 'custom)
;;; custom.el ends here
