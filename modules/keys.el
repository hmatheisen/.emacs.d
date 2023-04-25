;;; keys.el --- Some key (re)bindings                -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Henry MATHEISEN

;; Author: Henry MATHEISEN <haineriz@posteo.de>
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

;; Cool kill line
(defun kill-line-to-begining ()
  "Kill current line to position 0."
  (interactive)
  (kill-line 0))
(global-set-key (kbd "s-<backspace>") 'kill-line-to-begining)

;; Escape is quit
(global-set-key (kbd "<escape>") 'keyboard-quit)

;; Vim keybindings if needed
(use-package evil
  :init
  ;; C-u is Vi behaviour
  (setq evil-want-C-u-scroll t)
  ;; Default mode is emacs
  (setq evil-default-state 'emacs)
  ;; Override list of buffers where default state is something else than "emacs"
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)
  (setq evil-normal-state-modes '(ruby-mode typescript-mode web-mode))
  ;; Use Emacs binding in insert state
  (setq evil-disable-insert-state-bindings t)
  ;; Don't change cursor in insert mode
  (setq evil-insert-state-cursor nil)
  :config
  ;; Customize undo system
  (evil-set-undo-system 'undo-tree)
  ;; Use SPC instead of : for commands
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") 'evil-ex))
  ;; Start evil mode
  (evil-mode 1))


(provide 'keys)
;;; keys.el ends here
