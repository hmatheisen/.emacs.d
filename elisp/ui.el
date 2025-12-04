;;; ui.el --- UI related config                      -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Henry MATHEISEN

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(setq-default ns-use-proxy-icon nil)

(use-package modus-themes
  :ensure t
  :config
  (modus-themes-load-theme 'modus-vivendi)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle)
  :custom
  ((modus-themes-common-palette-overrides
    '((fg-line-number-active fg-main)
      (fg-line-number-inactive fg-dim)
      (bg-line-number-active bg-hl-line)
      (bg-line-number-inactive unspecified)

      (bg-mode-line-active bg-lavender)
      (border-mode-line-active indigo)

      (comment cyan-faint)
      (builtin magenta-warmer)
      (constant blue-cooler)
      (fnname magenta)
      (keyword magenta-cooler)
      (property cyan)
      (type cyan-cooler)
      (string blue-warmer)
      (variable cyan)

      (fringe unspecified)
      (cursor magenta-intense)
      (bg-region bg-cyan-intense)
      (bg-hl-line bg-blue-subtle)))
   (modus-vivendi-palette-overrides
    '((bg-main "#1f1f1f")
      (fg-main "#eeeeee")))
   (modus-themes-completions nil)
   (modus-themes-to-toggle
    '(modus-vivendi
      modus-operandi-tinted))
   (modus-themes-bold-constructs nil)
   (modus-themes-italic-constructs nil)
   (modus-themes-variable-pitch-ui nil)))

;; TODO:
;; FIXME:
;; NOTE:
(defface todo-face
  `((t (:foreground ,(modus-themes-get-color-value 'red))))
  "TODO keyword highlight.")

(defface fixme-face
  `((t (:foreground ,(modus-themes-get-color-value 'yellow-warmer))))
  "FIXME keyword highlight.")

(defface note-face
  `((t (:foreground ,(modus-themes-get-color-value 'green-warmer))))
  "NOTE keyword highlight.")

;; Mode line

;; Replaced the original var with one without minor mode list
(setq mode-line-modes
      (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
        (list (propertize "%[" 'help-echo recursive-edit-help-echo)
	          "("
	          `(:propertize ("" mode-name)
			                help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
			                mouse-face mode-line-highlight
			                local-map ,mode-line-major-mode-keymap)
	          '("" mode-line-process)
	          (propertize
               "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
		       'mouse-face 'mode-line-highlight
		       'local-map (make-mode-line-mouse-map
				           'mouse-2 #'mode-line-widen))
	          ")"
	          (propertize "%]" 'help-echo recursive-edit-help-echo) " ")))

(setq-default mode-line-format
              (delete '(vc-mode vc-mode) mode-line-format))

(add-hook
 'compilation-filter-hook
 #'(lambda ()
     "Apply ansi color filter on compilation buffer unless it is a *grep*
buffer."
     (when (not (string= (buffer-name) "*grep*"))
       (ansi-color-compilation-filter))))

(define-layout elevo
  "Run elevo rails in main vertical layout."
  :project-path "~/Code/elevo-rails/"
  :procs '("start-rails-server" "start-sidekiq" "start-client")
  :layout :main-vertical
  :proc-fn #'vterm-start-process)

(provide 'ui)
;;; ui.el ends here
