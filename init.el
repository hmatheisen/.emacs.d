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

;;

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-insert-mode t)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(completion-auto-help 'visible)
 '(completion-auto-select 'second-tab)
 '(completion-ignore-case t t)
 '(completion-preview-minimum-symbol-length 2)
 '(completion-styles '(basic flex orderless) nil nil "Customized with use-package orderless")
 '(completions-format 'one-column)
 '(completions-max-height 15)
 '(completions-sort 'alphabetical)
 '(confirm-kill-emacs 'y-or-n-p)
 '(context-menu-mode t)
 '(default-frame-alist '((ns-transparent-titlebar . t)))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(display-line-numbers nil)
 '(display-line-numbers-width 4)
 '(dynamic-completion-mode t)
 '(ediff-merge-split-window-function 'split-window-horizontally)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-ignored-server-capabilities '(:inlayHintProvider))
 '(eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
 '(electric-pair-mode t)
 '(enable-recursive-minibuffers nil)
 '(epg-pinentry-mode 'loopback)
 '(fill-column 80)
 '(global-auto-revert-mode t)
 '(global-completion-preview-mode nil)
 '(global-goto-address-mode t)
 '(global-page-break-lines-mode t nil (page-break-lines))
 '(global-so-long-mode t)
 '(grep-use-null-device nil)
 '(indent-tabs-mode nil)
 '(ispell-program-name "aspell")
 '(mode-line-compact 'long)
 '(ns-antialias-text t)
 '(ns-use-fullscreen-animation t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(aidermacs autothemer cape centaur-tabs cider closql consult copilot
               copilot-chat corfu crontab-mode csv-mode diredfl doric-themes
               ef-themes emmet-mode evil exec-path-from-shell fido-vertical-mode
               flymake-eslint gcmh ghub glsl-mode google-translate gptel helpful
               ibuffer-project inf-ruby magit marginalia modus-themes
               multiple-cursors nerd-icons ns-auto-titlebar olivetti orderless
               org-present page-break-lines prettier rails-log-mode
               rainbow-delimiters rbs-mode rg rich-minority sass-mode
               shrink-path slime standard-themes treemacs vertico
               visual-fill-column vlf vterm vundo yaml yaml-mode yasnippet))
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch "main")))
 '(pixel-scroll-precision-mode t)
 '(recentf-max-menu-items 100)
 '(recentf-mode t)
 '(repeat-mode t)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values '((flymake-mode)))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(scroll-preserve-screen-position 1)
 '(show-paren-delay 0)
 '(tab-width 4)
 '(text-scale-mode-step 1.1)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(trash-directory "~/.Trash")
 '(truncate-lines t)
 '(use-package-always-ensure t)
 '(user-mail-address "henry.mthsn@gmail.com")
 '(warning-minimum-level :emergency)
 '(windmove-default-keybindings '([ignore]))
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :height 120 :family "Hack"))))
 '(error ((t :underline nil)))
 '(fixed-pitch ((t (:inherit 'default :familiy "Hack"))))
 '(fixed-pitch-serif ((t (:inherit 'default :familiy "Hack"))))
 '(flymake-error ((t (:underline nil))))
 '(flymake-note ((t (:underline nil))))
 '(flymake-warning ((t (:underline nil))))
 '(info-title-1 ((t (:height 1.7))))
 '(info-title-2 ((t (:height 1.5))))
 '(info-title-3 ((t (:height 1.3))))
 '(info-title-4 ((t (:height 1.1))))
 '(variable-pitch ((t (:inherit 'default :family "Noto Sans"))))
 '(warning ((t :underline nil))))

;;; Consts

(defconst *is-a-mac* (eq system-type 'darwin))


;;; Load my own lisp code

;; Load lisp code in other directories
(let ((path (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path path)
  (add-to-list 'elisp-flymake-byte-compile-load-path path))


;;; System

(setq user-mail-address "henry.mthsn@gmail.com"
      smtpmail-smtp-service 587
      smtpmail-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it)

(setq gnus-select-method
      '(nnimap "imap.gmail.com"))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(defun authinfo-password-for (host)
  "Fetch the password for HOST in authinfo.gpg file."
  (let ((source (auth-source-search :host host)))
    (cond ((not source)
           (error (concat "There are no sources with the name: " host)))
          ((length< source 1)
           (error (concat "There is more than one source with the name: " host))))
    (auth-info-password (car source))))

(setq erc-nick "haineriz"
      erc-password (authinfo-password-for "libera")
      erc-user-full-name "Henry M."
      erc-user-login-name "haineriz"
      erc-fill-column 80)

;;; UI

(use-package ns-auto-titlebar
  :when *is-a-mac*
  :config
  (ns-auto-titlebar-mode))

;; Standard themes is the standard
;; (use-package standard-themes
;;   :config
;;   (standard-themes-load-theme 'standard-light-tinted)
;;   (define-key global-map (kbd "<f5>") #'standard-themes-toggle)
;;   :custom
;;   ((standard-themes-bold-constructs t)
;;    (standard-themes-italic-constructs t)
;;    (standard-themes-mixed-fonts t)
;;    (standard-themes-common-palette-overrides '((fringe unspecified)))
;;    (standard-themes-to-toggle '(standard-light-tinted standard-dark))))

;; Modus themes
(use-package modus-themes
  :config
  (modus-themes-load-theme 'modus-operandi-tinted)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle)
  :custom
  ((modus-themes-common-palette-overrides
    '(;; Make line numbers less intense
      (fg-line-number-active fg-main)
      (bg-line-number-inactive unspecified)
      (bg-line-number-active bg-hl-line)
      ;; Make the fringe invisible
      (fringe unspecified)))
   (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi))
   (modus-themes-bold-constructs t)
   (modus-themes-italic-constructs t)
   (modus-themes-variable-pitch-ui nil)))

;; Ef themes
(use-package ef-themes
  :custom
  (ef-themes-headings nil))

;; Doom themes
;; (use-package doom-themes
;;   :custom
;;   ((doom-themes-enable-bold t)
;;    (doom-themes-enable-italic t)
;;    (doom-gruvbox-dark-variant "hard")
;;    (doom-gruvbox-light-variant "soft"))
;;   :config
;;   (doom-themes-org-config))

;; Line numbers in prog mode only
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode t)
            (hl-line-mode t)))

(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

(use-package which-key
  :custom
  (which-key-idle-delay 1.0)
  :init
  (which-key-mode))

(use-package helpful
  :defer t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

;; Minibuffer

(define-advice keyboard-quit (:around (quit))
  "Quit the current context.

When there is an active minibuffer and we are not inside it close it.  When we
are inside the minibuffer use the regular `minibuffer-keyboard-quit' which quits
any active region before exiting.  When there is no minibuffer `keyboard-quit'
unless we are defining or executing a macro."
  (if (active-minibuffer-window)
      (if (minibufferp)
          (minibuffer-keyboard-quit)
        (abort-recursive-edit))
    (unless (or defining-kbd-macro
                executing-kbd-macro)
      (funcall-interactively quit))))

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("C-v" . vertico-scroll-up)
              ("M-v" . vertico-scroll-down))
  :custom
  (vertico-count 15))
;; (use-package icomplete
;;   :ensure nil
;;   :custom
;;   (fido-vertical-mode t))

(use-package marginalia
  :custom
  (marginalia-field-width 100)
  :init
  (marginalia-mode))

(use-package page-break-lines
  :config (global-page-break-lines-mode))

(use-package treemacs
  :custom
  (treemacs-no-png-images t)
  (treemacs-width 50)
  (treemacs-width-is-initially-locked nil)
  :bind (("s-t" . treemacs)))

(when *is-a-mac*
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-right-option-modifier 'nil))

;; Display buffer alist
(defun display-buffer-below (buffer-name)
  "Display BUFFER-NAME below the selected window."
  (add-to-list 'display-buffer-alist
               `(,buffer-name
                 (display-buffer-below-selected)
                 (window-height . 0.3))))

(display-buffer-below "\\*Flymake diag.+\\*")
(display-buffer-below "\\*eldoc.*\\*")

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)

(use-package ibuffer-project
  :config
  (add-hook
   'ibuffer-hook
   (lambda ()
     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
     (unless (eq ibuffer-sorting-mode 'project-file-relative)
       (ibuffer-do-sort-by-project-file-relative)))))

;; Clean modeline
;; Remove `vc-mode' from modeline
(setq-default mode-line-format
              (delete '(vc-mode vc-mode) mode-line-format))

(use-package rich-minority
  :custom
  (rm-blacklist (mapconcat 'identity '() "\\|"))
  :config
  (rich-minority-mode t))

;; Line spacing
(setq-default line-spacing nil)

;; Clean title bar
(setq-default ns-use-proxy-icon nil)

;;; Completion & Navigation

;; Orderless completion mode
(use-package orderless
  :ensure t
  :config
  (add-hook 'icomplete-minibuffer-setup-hook
            (lambda ()
              (setq-local completion-styles '(orderless basic flex))))
  ;; Disable SPC completion in minibufffer since we use if for orderless
  (define-key minibuffer-local-completion-map " " 'self-insert-command)
  :custom
  (completion-styles '(orderless basic flex)))

;; Replace dabbrev-expand with hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(defun hippie-try-function-dabbrev-first ()
  "Reorder the function lists in `hippie-expand-try-functions-list' to put all
dabbrev functions first."
  (let ((groups (seq-group-by           ; Maybe there's a better way to do this?
                 (lambda (fun)
                   (if (string-match "try-expand-dabbrev" (symbol-name fun))
                       "dabbrev"
                     "others"))
                 hippie-expand-try-functions-list)))
    (append
     (alist-get "dabbrev" groups nil nil #'string=)
     (alist-get "others" groups nil nil #'string=))))

(setq hippie-expand-try-functions-list (hippie-try-function-dabbrev-first))

;; In buffer completion
;; (use-package corfu
;;   :bind
;;   (:map corfu-map ("M-SPC" . corfu-insert-separator))
;;   :init
;;   (corfu-popupinfo-mode)
;;   (corfu-history-mode))

;; Completion at point extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dict))

(use-package yasnippet
  :custom (yas-indent-line 'fixed)
  :config (yas-global-mode t))

;; Search and navigation
(use-package consult
  :bind
  (("C-c m"             . consult-man)
   ("C-c i"             . consult-info)
   ([remap Info-search] . consult-info)
   ("C-x b"             . consult-buffer) ;; TODO: Maybe switch (kbd "s-b") later
   ("C-x 4 b"           . consult-buffer-other-window)
   ("C-x 5 b"           . consult-buffer-other-frame)
   ("C-x t b"           . consult-buffer-other-tab)
   ("C-x r b"           . consult-bookmark)
   ("C-x p b"           . consult-project-buffer) ;; TODO: idem
   ("M-y"               . consult-yank-pop)
   ("M-g f"             . consult-flymake)
   ("M-g g"             . consult-goto-line)
   ("M-g M-g"           . consult-goto-line)))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))


;;; Files

;; Backup files live in user emacs directory
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Auto save files live in temporary file directory
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Dired
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-q" . wdired-change-to-wdired-mode))
  :config
  (when (executable-find "gls")
    (setq insert-directory-program "gls"
          dired-listing-switches "-aGFhlv --dired --group-directories-first
          --time-style=long-iso"))
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  ;; Guess a default target directory
  (setq dired-dwim-target t))

(use-package diredfl
  :config (diredfl-global-mode))

;; Quicker switch to buffer
(global-set-key (kbd "s-b") 'switch-to-buffer)

;; Invoke recentf
(global-set-key (kbd "s-r") 'recentf)

;; Ripgrep
(use-package rg
  :config
  (rg-enable-default-bindings)
  (rg-define-toggle "--context=10" "C" nil)
  (rg-define-search project-search-regex
    "Search in project using regex."
    :query ask
    :format regexp
    :files "everything"
    :dir project)
  (define-key project-prefix-map (kbd "g") 'project-search-regex))

;;; Languages

;; Eglot
(use-package eglot
  :ensure nil
  :preface
  (defun setup-other-flymake-backends ()
    "Add other backends to flymake when using eglot"
    (cond ((derived-mode-p 'typescript-ts-mode 'tsx-ts-mode)
           (flymake-eslint-enable))
          ((derived-mode-p 'ruby-ts-mode)
           (add-hook 'flymake-diagnostic-functions
                     'ruby-flymake-auto)))) ; Enables rubocop
  :hook ((ruby-ts-mode       . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode        . eglot-ensure)
         (c++-ts-mode        . eglot-ensure)
         (go-ts-mode         . eglot-ensure)
         (eglot-managed-mode . setup-other-flymake-backends))
  :config
  ;; (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))
  )

;; TS/TSX
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(use-package emmet-mode
  :hook (tsx-ts-mode . emmet-mode))

(define-skeleton react-component-skeleton
  "React component."
  "Component name: "
  "import React from 'react';\n\n"
  "type Props = {};\n\n"
  "const " str " = ({}: Props) => {\n"
  "  return <p>" str "</p>;\n";
  "};\n\n"
  "export default " str ";")
(define-auto-insert
  '("\\.tsx\\'" . "React component")
  'react-component-skeleton)

;; Sass
(use-package sass-mode)

;; Enable eslint for flymake
(use-package flymake-eslint)

;; Flymake
(use-package flymake
  :bind
  (:map flymake-mode-map
        ("M-n" . 'flymake-goto-next-error)
        ("M-p" . 'flymake-goto-prev-error)
        ("M-g f" . 'flymake-show-buffer-diagnostics)))

;; YAML mode
(use-package yaml-mode)

;; Markdown
(use-package markdown-mode
  :custom ((markdown-command "md2html"))
  :hook
  ((markdown-mode . (lambda ()
                      ;; Do not remove trailing whitespace when cleaning in
                      ;; markdown mode
                      (setq-local whitespace-style
                                  (delq 'trailing whitespace-style)))))
  :config
  (define-skeleton markdown-release-skeleton
    "Release template."
    "Insert Release Tag: "
    "# " str "\n\n"
    "## Stories\n\n"
    "## Bugfixes\n\n"
    "## Tasks\n\n"
    "## Migrations\n\n"
    "## Rollback Plan\n")
  ;; Use `markdown-release' as auto-insert on 'release.md' files.
  (define-auto-insert
    '("release.md" . "Release template") 'markdown-release-skeleton))

;; Treesit
(defvar treesit-language-source-alist
  '((ruby "https://github.com/tree-sitter/tree-sitter-ruby")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (bash "https://github.com/tree-sitter/tree-sitter-bash")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun install-treesit-languages ()
  "Install all treesit languages."
  (interactive)
  (mapc (lambda (lang-alist)
          (let ((lang (car lang-alist)))
            (unless (treesit-language-available-p lang)
              (message "Language %s not available" lang))
            (treesit-install-language-grammar lang)))
        treesit-language-source-alist))

;; Remap major modes to their treesit equivalent
(setq major-mode-remap-alist
      '((ruby-mode       . ruby-ts-mode)
        (tsx-mode        . tsx-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode       . json-ts-mode)
        (dockerfile-mode . dockerfile-ts-mode)
        (c++-mode        . c++-ts-mode)
        ;; (yaml-mode       . yaml-ts-mode)
        (go-mode         . go-ts-mode)))

;; Dockerfile
;; Somehow, this needs to be required otherwise the auto-mode -> mode-remap
;; won't work.
(require 'dockerfile-ts-mode)

;; Golang
(setq go-ts-mode-indent-offset 4)
(add-hook 'go-ts-mode-hook
          (lambda ()
            (setq-local fill-column 100)
            (display-fill-column-indicator-mode t)
            (local-set-key (kbd "C-c f") 'eglot-format-buffer)))

;; Ruby auto insert header on new files
(define-auto-insert
  '("\\.rb\\'" . "Ruby frozen string header")
  '(nil
    "# frozen_string_literal: true\n"
    "\n"))

(add-hook 'ruby-ts-mode-hook
          (lambda ()
            (setq fill-column 100)))

;; C/C++
(add-hook 'c-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode t)
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq c-basic-offset 4)))
(add-hook 'c++-ts-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode t)
            (local-set-key (kbd "C-c f") 'eglot-format-buffer)))

;; Lisp
(use-package slime
  :config
  (setq inferior-lisp-program "sbcl --dynamic-space-size 4096")
  (add-hook 'slime-mode-hook
            (lambda ()
              (setq slime-completion-at-point-functions
                    '(slime-simple-completion-at-point t))))
  :custom
  (slime-completion-at-point-functions '(slime-simple-completion-at-point t)))

;;; Site lisp config

;; Run commands in given directories
(require 'tasks)

(define-task rails-migrate
  :command "bundle exec rails db:migrate"
  :description "Run migrations in rails project"
  :project-path "~/Code/elevo-rails/")

(define-task rails-gettext
  :command "bundle exec rails gettext:update"
  :description "Regenerate translation files in rails project"
  :project-path "~/Code/elevo-rails/")

(define-task sort-forestschema
  :command "bundle exec bin/sort_forestschema"
  :project-path "~/Code/elevo-rails/"
  :description "Sort `.forestadmin-schema.json' file"
  :async nil)

(define-task annotate-models
  :command "bundle exec rails annotate_models"
  :project-path "~/Code/elevo-rails/"
  :description "Annotate rails models")

(define-task pre-commit
  :command "pre-commit run --all-files"
  :project-path "~/Code/elevo-rails"
  :description "Run all pre-commit checks against all files")

;; Format buffer with the given command
(require 'format-buffer)

(format-lang ruby-ts
  :command "stree"
  :args '("format" "--print-width=100" "--plugin=plugin/trailing_comma"))

(format-lang nxml
  :command "xmllint"
  :args '("--format" "-"))

(format-lang sql
  :command "pg_format")

(format-lang c
  :command "clang-format")

(use-package prettier
  :config
  (let ((prettify-hook (lambda ()
                         (local-set-key (kbd "C-c f") 'prettier-prettify))))
    (add-hook 'typescript-ts-mode-hook prettify-hook)
    (add-hook 'tsx-ts-mode-hook prettify-hook)))

;;; Org

(use-package org
  :ensure nil
  :preface
  (defun my-org-mode-hook ()
    ;; (org-indent-mode 1)
    (visual-line-mode 1)
    (windmove-mode -1)
    ;; Bigger font for title and levels
    ;; It lives here since a lot of themes like to override this
    (set-face-attribute 'org-document-title nil :height 1.7)
    (set-face-attribute 'org-level-1 nil :height 1.5)
    (set-face-attribute 'org-level-2 nil :height 1.3)
    (set-face-attribute 'org-level-3 nil :height 1.1))
  :hook ((org-mode . my-org-mode-hook))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  ;; files
  (setq org-directory "~/org/")
  (setq org-agenda-files (list org-directory))
  ;; others
  (setq org-startup-folded 'fold)
  (setq org-log-done 'time)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "ON GOING(g)" "|" "DONE(d)" "WON'T DO(w)")
          (sequence "TODO(T)" "IN PROGRESS(P)" "TO REVIEW(R)" "TO TEST(F)"
                    "READY TO MERGE(M)" "|" "DONE(D)")))
  (setq org-capture-templates
        '(("i" "Emacs Ideas" entry
           (file+headline (lambda () (concat org-directory "journal.org"))
                          "Ideas")
           "* %?\nEntered on: %u")
          ("m" "Merge Request" entry
           (file (lambda () (concat org-directory "tasks.org"))
                 "Merge Requests")
           "* %?\n")
          ("t" "Todo" entry
           (file (lambda () (concat org-directory "tasks.org")))
           "* TODO %?\n")
          ("T" "Ticket" entry
           (file+headline (lambda () (concat org-directory "tasks.org"))
                          "Tickets")
           "* IN PROGRESS %?\nSCHEDULED: %t")))
  (define-skeleton org-refinement-skeleton
    "Refinement Template."
    "Insert Refinement subject: "
    "#+TITLE: " str "\n"
    "#+OPTIONS: html-postamble:nil num:nil toc:nil\n"
    "#+HTML_HEAD: <link rel=\"stylesheet\" href=\"../org.css\">\n\n"
    "* Notes :noexport:\n"
    "* Summary\n"
    "** Catch phrase\n"
    "** Feature Flag\n"
    "** Links\n"
    "- Link to product discovery\n"
    "- Link to Figma\n"
    "- Link to Jira epic\n"
    "* Tech Solution\n"
    "** Development steps\n"
    "** Models\n"
    "** Services\n"
    "** Controllers\n"
    "** Serializers\n"
    "** Policies/Abilities\n"
    "** Scenes\n"
    "** Components\n"
    "* Questions\n"
    "** Product\n"
    "** Tech\n"
    "* Tickets\n")
  (define-auto-insert
    '("refinements/.*\.org" . "Refinement template")
    'org-refinement-skeleton)
  (define-skeleton org-tmp-skeleton
    "tmp notes/to be imported elsewhere."
    "Insert title: "
    "# -*- eval: (auto-fill-mode -1) -*-"
    "#+TITLE: " str "\n"
    "#+DATE: " "<" (format-time-string "%Y-%m-%d") ">" "\n"
    "#+OPTIONS: toc:nil\n\n")
  (define-auto-insert
    '("tmp/.*\.org" . "Tmp notes")
    'org-tmp-skeleton)
  (define-auto-insert
    '("tickets/.*\.org" . "Org ticket")
    '(nil
      "#+TITLE: "
      (humanize-branch-name
       (file-name-sans-extension
        (car (last (split-string (buffer-file-name) "/")))))
      "\n")))

(use-package org-present)

;;; Text

(use-package google-translate
  :config
  (setq google-translate-translation-directions-alist '(("en" . "fr")
                                                        ("fr" . "en")))
  :bind
  (("C-c t" . google-translate-smooth-translate)))

(require 'isearch-transient)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'scroll-left 'disabled nil)

(defun align-equals (beg end)
  "Align `=' signs in a given region, from BEG to END."
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)="))

;; Whitespace cleanup on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Auto insert mode
(auto-insert-mode t)

;; Redo binding with super
(global-set-key (kbd "s-Z") 'undo-redo)

;; Kill to end of line
(defun kill-beg-line ()
  "Kill a line from point to column 0."
  (interactive)
  (kill-line 0))

(global-set-key (kbd "s-<backspace>") 'kill-beg-line)

;; Auto fill in text mode
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Abbrev mode by default in all buffers
(setq-default abbrev-mode t)

(use-package wgrep
  :custom
  (auto-save-buffer t))

(defun kill-ring-save-whole-buffer ()
  "Save whole buffer to the kill ring."
  (interactive)
  (kill-ring-save
   (point-min)
   (point-max))
  (message "Buffer saved to kill ring."))

(global-set-key (kbd "C-x C-y") 'kill-ring-save-whole-buffer)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(global-set-key (kbd "M-j") 'join-line)

(use-package vundo)

;; Support for very large files
(use-package vlf
  :config
  (defun display-ansi-colors ()
    "Display ansi colors as actual colors in buffers.  Works well in pair with
vlf to see long log files."
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))


;;; Project

(define-key global-map (kbd "s-p") project-prefix-map)

(defmacro with-current-project (project &rest body)
  "Execute BODY with ROOT as the current project root."
  (declare (indent 1))
  `(let ((,project (project-current t)))
     ,@body))

;; Copy file absolute path
(defun project-absolute-file-path ()
  "Print and kill the absolute file path of the current buffer in a project."
  (interactive)
  (unless buffer-file-name
    (error "project-absolute-file-path: could not get buffer file name"))
  (with-current-project project
    (let* ((root (project-root project))
           (absolute-file-path (file-relative-name buffer-file-name root)))
      (kill-new absolute-file-path)
      (message (concat "Saved \"" absolute-file-path "\" to kill ring")))))

(define-key project-prefix-map "\C-y" 'project-absolute-file-path)

;; open vterm at root of project
(defun project-vterm ()
  "Open a vterm at the root of the current project."
  (interactive)
  (with-current-project project
    (let ((name (project-name project))
          (root (project-root project)))
      (when (vterm-open (concat "*vterm*<" name ">"))
        (vterm-send-string (concat "cd " root))
        (vterm-send-return)
        (vterm-clear)))))

(define-key project-prefix-map "t" 'project-vterm)

(defun project-rails-console ()
  "Open a rails console at the root of the current project."
  (interactive)
  (with-current-project project
    (let ((name (project-name project))
          (root (project-root project)))
      (when (vterm-open (concat "*vterm*<" name " - rails console>"))
        (vterm-send-string (concat "cd " root " && bin/rails c"))
        (vterm-send-return)
        (vterm-clear)))))

(define-key project-prefix-map (kbd "C-r") 'project-rails-console)

(defun project-clean-buffers ()
  "Like `project-kill-buffers' but keeps some arbitrary ones."
  (interactive)
  (save-some-buffers)
  (let* ((project (project-current t))
         (project-name (project-name project))
         (project-root (project-root project))
         (project-buffers (project-buffers project))
         (dired-buffer-name (buffer-name (dired-noselect project-root))))
    (mapc (lambda (buffer)
            (let ((buffer-name (buffer-name buffer)))
              (unless (or (string-equal buffer-name dired-buffer-name)
                          (string-equal buffer-name (concat "magit: " dired-buffer-name))
                          (string-match-p "^\*vterm\*" buffer-name)
                          (string-match-p "*EGLOT" buffer-name))
                (kill-buffer buffer))))
          project-buffers)
    (message (concat "Buffers cleaned for project: " project-name))))

(define-key project-prefix-map (kbd "k") 'project-clean-buffers)

;;; Git

(use-package transient)

(use-package magit
  :bind (("C-x g" . 'magit-status)
         :map magit-file-section-map
         ("RET" . magit-diff-visit-file-other-window)
         :map magit-hunk-section-map
         ("RET" . magit-diff-visit-file-other-window))
  :config
  (defmacro with-current-branch (branch &rest body)
    "Execute BODY with BRANCH set to to the current VC branch."
    (declare (indent 1))
    `(if-let ((,branch (magit-get-current-branch)))
         (progn ,@body)
       (error "Not in a git repository")))
  (transient-append-suffix 'magit-log "-A" '("-m" "No Merges" "--no-merges")))

(defun kill-ring-save-forestadmin-url ()
  "Saves to kill ring the URL for Forestadmin of the current branch."
  (interactive)
  (with-current-branch branch
    (let ((url (concat "https://app.forestadmin.com/Elevo/" branch)))
      (kill-new url)
      (message (concat "Saved '" url "' to kill ring.")))))

(defun org-make-file-for-branch ()
  "Create a new org file for the current branch"
  (interactive)
  (with-current-branch branch
    (find-file (concat org-directory "tickets/" branch ".org"))))

;; (use-package forge
;;   :after magit)

;;; Utils

(defun add-to-path (path)
  "Add a path to variable `exec-path' and Emacs \"PATH\" variable."
  (add-to-list 'exec-path path)
  (setenv "PATH" (concat (getenv "PATH") ":" path)))

(defun chez-scheme-set-env (dir)
  "List dirs in chez scheme lib DIR and set the proper env."
  (let* ((scheme-lib-dirs
          (directory-files dir t
                           directory-files-no-dot-files-regexp))
         (env (string-join scheme-lib-dirs ":")))
    (setenv "CHEZSCHEMELIBDIRS" (concat env ":."))))

(add-to-path "/usr/local/bin")
(add-to-path "/Library/TeX/texbin")
(add-to-path "/Users/henry/.rbenv/shims")
(add-to-path "/Users/henry/.local/bin")

(when *is-a-mac*
  (setenv "DYLD_LIBRARY_PATH" "/opt/homebrew/lib")
  (setenv "LIBRARY_PATH" "/opt/homebrew/lib")
  (setenv "C_PATH" "/opt/homebrew/include"))

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

(defun repeat-last-async-shell-command ()
  "Repeats the last shell command in as an `async-shell-command'."
  (interactive)
  (async-shell-command (car shell-command-history)))

(defun humanize-branch-name (branch-name)
  "Very personal way of transforming a BRANCH-NAME into its humanized name.
Example:
(humanize-branch-name \"fix-3456--fix-broken-stuff\")
\"FIX-3456: Fix broken stuff\"

When called interactively, choose from `magit-list-local-branch-names' and the humanized
name is saved to the kill ring"
  (interactive (list
                (completing-read
                 "Branch name: " (magit-list-local-branch-names))))
  (let* ((split (split-string branch-name "-"))
         (ticket-type (nth 0 split))
         (ticket-id (nth 1 split))
         (description (nthcdr 3 split))
         (humanized (concat (upcase ticket-type)
                            "-"
                            ticket-id
                            ": "
                            (capitalize (car description))
                            " "
                            (string-join (cdr description) " "))))
    (when (called-interactively-p)
      (kill-new humanized)
      (message (concat "Saved \"" humanized "\" to kill ring.")))
    humanized))

;;; Window

;; Move between windows
(defun other-window-backward ()
  "Move to previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-window-backward)

(defun window-half-height ()
  "Return half the height of a window."
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-half-page-up-command ()
  "Scroll up half the height of a window."
  (interactive)
  (scroll-up-command (window-half-height)))

(defun scroll-half-page-down-command ()
  "Scroll down half the height of a window."
  (interactive)
  (scroll-down-command (window-half-height)))

(global-set-key (kbd "C-v") 'scroll-half-page-up-command)
(global-set-key (kbd "M-v") 'scroll-half-page-down-command)

(defun split-window-right-focus ()
  "Splits whe window below and move point to new window."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun split-window-below-focus ()
  "Splits whe window below and move point to new window."
  (interactive)
  (split-window-below)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'split-window-below-focus)
(global-set-key (kbd "C-x 3") 'split-window-right-focus)

;; Unbind suspend frame in GUI mode
(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-<wheel-up>"))
  (global-unset-key (kbd "C-<wheel-down>")))


;;; Tools

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :bind (:map copilot-completion-map
              ("C-<return>" . copilot-accept-completion)
              ("C-RET" . copilot-accept-completion)))

(use-package copilot-chat)

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (add-hook 'aidermacs-before-run-backend-hook
            (lambda ()
              (setenv "ANTHROPIC_API_KEY"
                      (authinfo-paswosrd-for "anthropic-api-key")))))

(use-package gptel
  :config
  (setq gptel-backend
        (gptel-make-gemini "Gemini"
          :key (authinfo-password-for "gemini-api-key")
          :stream t)))

(use-package vterm
  :config
  (defun vterm-open (buffer-name)
    "Open or switch to a vterm buffer called BUFFER-NAME.  Return `nil' if the
  buffer already existed otherwise, return the new buffer."
    (if-let ((buffer (seq-find (lambda (buffer)
                                 (string= buffer-name (buffer-name buffer)))
                               (buffer-list))))
        (progn (switch-to-buffer buffer) nil)
      (vterm buffer-name)))
  (defun vterm-start-process (proc)
    "Open or switch to a vterm buffer to run a command PROC."
    (let ((vterm-buffer-name (concat "*vterm*<" proc ">")))
      (when (vterm-open vterm-buffer-name)
        (vterm-send-string proc)
        (vterm-send-return)))))

(require 'emacsocil)

(define-layout elevo
  :project-path "~/Code/elevo-rails/"
  :procs '("start-rails-server" "start-sidekiq" "start-client")
  :layout :main-vertical
  :proc-fn #'vterm-start-process)

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init)

;;; init.el ends here
