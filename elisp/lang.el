;;; lang.el --- language config                      -*- lexical-binding: t; -*-

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

(defun hma/prog-mode-hook ()
  "Prog mode hook."
  (flymake-mode t)
  (hl-line-mode t)
  (auto-fill-mode t)
  (display-line-numbers-mode t)
  (display-fill-column-indicator-mode t)
  (corfu-mode)

  (keymap-local-set "M-n" 'flymake-goto-next-error)
  (keymap-local-set "M-p" 'flymake-goto-prev-error)

  (font-lock-add-keywords
   nil
   '(("\\<\\(TODO\\):" 1 'todo-face prepend)
     ("\\<\\(FIXME\\):" 1 'fixme-face prepend)
     ("\\<\\(NOTE\\):" 1 'note-face prepend))))

(add-hook 'prog-mode-hook #'hma/prog-mode-hook)

;; C/C++
(defconst hma/c-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . t)
    (c-hanging-braces-alist . ((statement . (before after))))
    (c-hanging-colons-alist . ((access-label . (after))
                               (case-label . (after))
                               (inher-intro)
                               (label . (after))
                               (member-init-intro)))
    (c-cleanup-list . (empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator
                       one-liner-defun
                       compact-empty-funcall
                       comment-close-slash))
    (c-offsets-alist . ((substatement-open . 0)
                        ;; (innamespace . 0)
                        ))
    (c-hanging-semi&comma-criteria . ((lambda () 'stop))))
  "C/C++ style.")

(c-add-style "hma" hma/c-style)

;; C
(add-hook 'c-mode-hook 'eglot-ensure)

;; C++
(add-to-list
 'eglot-server-programs
 '(c++-mode . ("clangd"
               "--background-index"
               "-j=12"
               "--clang-tidy"
               "--all-scopes-completion"
               "--completion-style=detailed"
               "--header-insertion-decorators"
               "--header-insertion=iwyu")))

(defun hma/c++-hook ()
  "C++ mode hook."
  ;; FIXME: re-enable when indent is fixed
  (eglot-ensure)
  (setq fill-column 100)
  (c-set-style "hma")
  (c-toggle-auto-newline t)
  (c-toggle-auto-hungry-state t))
(add-hook 'c++-mode-hook 'hma/c++-hook)

(require 'c-ts-mode)
(require 'c++-ts-mode)

(setopt c-ts-mode-indent-offset 4)
(c-ts-mode-set-global-style "bsd")

;; Ruby
(add-to-list 'treesit-language-source-alist
             '(ruby . ("https://github.com/tree-sitter/tree-sitter-ruby")))
(add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
(add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))

(defun hma/ruby-hook ()
  "Ruby mode hook."
  (eglot-ensure)
  (setq fill-column 100
        comment-column 100))
(add-hook 'ruby-ts-mode-hook 'hma/ruby-hook)

;; TSX
(require 'typescript-ts-mode)
(add-to-list 'treesit-language-source-alist
             '(tsx ("https://github.com/tree-sitter/tree-sitter-typescript"
                    "master" "tsx/src")))
(defun hma/tsx-hook ()
  "Tsx mode hook."
  (setq comment-column 100)
  (eglot-ensure))
(add-hook 'tsx-ts-mode-hook 'hma/tsx-hook)

;; Typescript
(add-to-list 'treesit-language-source-alist
             '(ts ("https://github.com/tree-sitter/tree-sitter-typescript"
                   "master" "typescript/src")))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(defun hma/typescript-hook ()
  "Typescript mode hook."
  (setq comment-column 100)
  (eglot-ensure))
(add-hook 'typescript-ts-mode-hook 'hma/ruby-hook)

;; Javascript
(require 'js)
(setopt js-indent-level 2)
(add-to-list 'treesit-language-source-alist
             '(javascript . ("https://github.com/tree-sitter/tree-sitter-javascript")))
(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
(add-hook 'js-ts-mode-hook 'eglot-ensure)

;; Sass
(use-package sass-mode
  :ensure t)

;; Go
(require 'go-ts-mode)
(add-to-list 'treesit-language-source-alist
             '(go . ("https://github.com/tree-sitter/tree-sitter-go")))
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(setopt go-ts-mode-indent-offset 4)

;; Dart
(add-hook 'dart-mode 'eglot-ensure)
(font-lock-add-keywords
 'dart-mode
 '(("\\<\\(late\\)\\>" . 'font-lock-keyword-face)))

;; Python
(require 'python)
(add-to-list 'treesit-language-source-alist
             '(python . ("https://github.com/tree-sitter/tree-sitter-python")))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-hook 'python-ts-mode-hook 'eglot-ensure)

;; YAML
(require 'yaml-ts-mode)

;; JSON mode
(require 'json-ts-mode)
(add-to-list 'treesit-language-source-alist
             '(json . ("https://github.com/tree-sitter/tree-sitter-json")))
(add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))

;; Markdown
(use-package markdown-mode
  :custom ((markdown-command "md2html")))

;; Text mode
(defun hma/text-mode-hook ()
  "Text mode hook."
  (visual-line-mode t))

(add-hook 'text-mode-hook #'hma/text-mode-hook)

(define-task rails-migrate
  "Run migrations in rails project."
  :command "bundle exec rails db:migrate"
  :project t)

(define-task rails-gettext
  "Regenerate translation files in rails project."
  :command "bundle exec rails gettext:update"
  :project t)

(define-task sort-forestschema
  "Sort `.forestadmin-schema.json' file."
  :command "bundle exec bin/sort_forestschema"
  :path "~/Code/elevo-rails/"
  :async nil)

(define-task annotate-models
  "Annotate rails models."
  :command "bundle exec rails annotate_models"
  :project t)

(define-task pre-commit
  "Run all pre-commit checks against all files."
  :command "pre-commit run --all-files"
  :project t)

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

(provide 'lang)
;;; lang.el ends here
