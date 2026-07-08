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
 '(backup-directory-alist '(("." . "~/.emacs.d/backups")))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(completion-auto-deselect t)
 '(completion-auto-help 'always)
 '(completion-auto-select 'second-tab)
 '(completion-eager-update t)
 '(completion-pcm-complete-word-inserts-delimiters t)
 '(completion-pcm-leading-wildcard t)
 '(completion-show-help nil)
 '(completion-styles '(basic partial-completion flex))
 '(completions-detailed t)
 '(completions-format 'vertical)
 '(completions-group t)
 '(completions-max-height 20)
 '(completions-sort 'historical)
 '(custom-safe-themes
   '("967c23e9ba179b80560774419f081df22e7674aac23c5c550b817e4a1ce7d058" default))
 '(default-frame-alist '((ns-transparent-titlebar . t) (fullscreen . maximized)))
 '(delete-selection-mode t)
 '(dired-listing-switches
   "-aFGhlv --dired --group-directories-first --time-style=long-iso")
 '(display-buffer-alist
   '(("\\*eldoc" display-buffer-below-selected (window-height . 0.2) (no-other-window . t)
      (no-delete-other-windows . t))))
 '(display-line-numbers-width 4)
 '(eglot-code-action-indications nil)
 '(eglot-documentation-renderer 'markdown-ts-view-mode)
 '(eglot-ignored-server-capabilities '(:inlayHintProvider))
 '(eldoc-echo-area-prefer-doc-buffer t)
 '(eldoc-echo-area-use-multiline-p t)
 '(electric-pair-mode t)
 '(enable-recursive-minibuffers t)
 '(fill-column 100)
 '(fit-frame-to-buffer t)
 '(frame-resize-pixelwise t)
 '(global-goto-address-mode t)
 '(global-so-long-mode t)
 '(grep-command "rg --no-heading -C 5 -e ")
 '(grep-use-headings t)
 '(grep-use-null-device nil)
 '(ibuffer-expert t)
 '(ibuffer-use-other-window t)
 '(icon-preference '(text))
 '(indent-tabs-mode nil)
 '(log-edit-hook
   '(log-edit-insert-changelog log-edit-show-files log-edit-maybe-show-diff))
 '(minibuffer-auto-raise t)
 '(minibuffer-completion-auto-choose t)
 '(minibuffer-depth-indicate-mode t)
 '(minibuffer-electric-default-mode t)
 '(mode-line-collapse-minor-modes '(not flymake-mode))
 '(mode-line-collapse-minor-modes-to "")
 '(mode-line-compact 'long)
 '(mode-line-format
   '("%e" mode-line-front-space
     (:propertize
      ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
       mode-line-window-dedicated)
      display (min-width (6.0)))
     mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
     (project-mode-line project-mode-line-format) "  " mode-line-modes mode-line-misc-info
     mode-line-end-spaces))
 '(modus-themes-bold-constructs t)
 '(modus-themes-common-palette-overrides
   '((fringe unspecified) (bg-line-number-active bg-hl-line) (bg-line-number-inactive unspecified)
     (cursor magenta-intense) (bg-mode-line-active bg-lavender) (border-mode-line-active indigo)
     (bg-region bg-cyan-intense)))
 '(modus-themes-italic-constructs t)
 '(modus-vivendi-palette-overrides '((bg-main "#101010") (fg-main "#eeeeee")))
 '(ns-right-alternate-modifier 'none)
 '(ns-use-fullscreen-animation t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/") ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(dictionary editorconfig eglot exec-path-from-shell ghostel less-css-mode magit marginalia
                markdown-ts-mode org page-break-lines terraform-mode transient treemacs which-key))
 '(pixel-scroll-mode t)
 '(pixel-scroll-precision-mode t)
 '(pixel-scroll-precision-use-momentum t)
 '(recentf-auto-cleanup 'never)
 '(recentf-mode t)
 '(repeat-mode t)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position 1)
 '(scroll-step 5)
 '(server-mode t)
 '(tool-bar-mode nil)
 '(treemacs-no-png-images t)
 '(treemacs-width 50)
 '(truncate-lines t)
 '(vc-allow-async-diff t)
 '(vc-allow-async-revert t)
 '(vc-allow-rewriting-published-history 'ask)
 '(vc-async-checkin t)
 '(vc-auto-revert-mode t)
 '(vc-handled-backends '(Git))
 '(which-key-mode t)
 '(windmove-default-keybindings '([ignore] meta super))
 '(window-resize-pixelwise t)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaspace Neon" :height 110))))
 '(error ((t (:underline nil))))
 '(fill-column-indicator ((t (:background nil :height 1))))
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
 '(variable-pitch ((t (:inherit default))))
 '(warning ((t (:underline nil)))))


;;; Packages

;; Enable theme.  Must be after customize if we want the palette overrides to take effect
(load-theme 'modus-vivendi)

;; Disable NS icons
(setq-default ns-use-proxy-icon nil)

;; Sync path with zsh
(use-package exec-path-from-shell
  :ensure t
  :functions (exec-path-from-shell-initialize)
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(use-package treemacs
  :ensure t
  :bind ("s-t" . treemacs))

(defun hma/prog-mode-hook ()
  "Hook function for `prog-mode'."
  ;; Enable minor modes
  (flymake-mode t)
  (hl-line-mode t)
  (auto-fill-mode t)
  (display-line-numbers-mode t)
  (display-fill-column-indicator-mode t)
  ;; Set keymaps
  (keymap-local-set "M-n" 'flymake-goto-next-error)
  (keymap-local-set "M-p" 'flymake-goto-prev-error))

(add-hook 'prog-mode-hook #'hma/prog-mode-hook)

(use-package page-break-lines
  :functions global-page-break-lines-mode
  :config (global-page-break-lines-mode))


;;; Languages

;; Require Markdown TS for Eglot's documentation
(require 'markdown-ts-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))

(defun hma/typescript-ts-mode-hook ()
  "Hook function for `typescript-ts-mode'."
  (eglot-ensure))

(add-hook 'typescript-ts-mode-hook #'hma/typescript-ts-mode-hook)

(defun hma/tsx-ts-mode-hook ()
  "Hook function for `tsx-ts-mode'."
  (eglot-ensure))

(add-hook 'tsx-ts-mode-hook #'hma/tsx-ts-mode-hook)

;;;; Biome

;; Flymake backend

(defvar-local hma/biome--flymake-proc nil)
(defvar-local hma/biome--flymake-regexp
  "^::\\(\\w+\\) title=\\(\.*\\),file=.*,line=\\([0-9]+\\).*col=\\([0-9]+\\).*::\\(.*\\)$")

(defun hma/biome-flymake (report-fn &rest _args)
  "Flymake backend for biome.
REPORT-FN is the report function to call from Flymake"
  ;; Check executable
  (unless (executable-find "biome")
    (flymake-log :error "Cannot find biome executable"))
  ;; Kill existing process
  (when (process-live-p hma/biome--flymake-proc)
    (kill-process hma/biome--flymake-proc))
  (let* ((source (current-buffer))
         (file-name (buffer-file-name source)))
    (save-restriction
      (widen)
      (setq
       hma/biome--flymake-proc
       (make-process
        :name "biome-flymake" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *biome-flymake*")
        :command `("npx" "biome" "lint" "--reporter" "github" ,file-name)
        :sentinel
        (lambda (proc _event)
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                (if (with-current-buffer source (eq proc hma/biome--flymake-proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (cl-loop
                       while (search-forward-regexp hma/biome--flymake-regexp nil t)
                       for type = (match-string 1)
                       for title = (match-string 2)
                       for line = (string-to-number (match-string 3))
                       for col = (string-to-number (match-string 4))
                       for description = (match-string 5)
                       for info = (cond ((string= type "notice") :note)
                                        ((string= type "warning") :warning)
                                        ((string= type "error") :error))
                       for message = (concat title ": " description)
                       for (beg . end) = (flymake-diag-region source line col)
                       when (and beg end)
                       collect (flymake-make-diagnostic source
                                                        beg
                                                        end
                                                        info
                                                        message)
                       into diags
                       finally (funcall report-fn diags)))
                  (flymake-log :warning "Canceling obsolete check %s" proc))
              (kill-buffer (process-buffer proc))))))))))

(defun hma/biome-setup-flymake-backend ()
  "Add Biome to flymake backends."
  (interactive)
  (add-hook 'flymake-diagnostic-functions #'hma/biome-flymake nil t))

(defun hma/eglot-managed-mode-hook ()
  "Hook function to run after Eglot is setup.
It is used to setup other flymake backends since Eglot overrides the list"
  (cl-case major-mode
    (typescript-ts-mode (hma/biome-setup-flymake-backend))
    (tsx-ts-mode (hma/biome-setup-flymake-backend))))

(add-hook 'eglot-managed-mode-hook #'hma/eglot-managed-mode-hook)

;; Compilation regexp

(add-to-list 'compilation-error-regexp-alist-alist
             '(biome "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\).*━$" 1 2 3))
(add-to-list 'compilation-error-regexp-alist 'biome)


;;; Keymaps settings

(keymap-global-set "M-/" 'hippie-expand)
(keymap-global-set "M-j" 'join-line)

(keymap-global-set "s-p" project-prefix-map)
(keymap-global-set "s-b" 'switch-to-buffer)
(keymap-global-set "s-r" 'recentf)

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


;;; Utils

(defun new-buffer (new-buffer-name)
  "Create a new buffer named NEW-BUFFER-NAME and switch to it."
  (interactive "sNew buffer name: ")
  (switch-to-buffer
   (concat "*" new-buffer-name "*")))

(global-set-key (kbd "C-x B") 'new-buffer)

(use-package ghostel
  :bind
  (("C-x m" . ghostel)
   :map project-prefix-map
   ("m" . ghostel-project)
   ("M" . ghostel-project-list-buffers)))

;;; init.el ends here
