;;; ui.el --- Emacs UI related config                -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Henry MATHEISEN

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

(defconst *mono-font* "Iosevka Term"
  "Default mono font to be used.")

(defconst *font-size* 16
  "Font size un points.")

;; Display column number
(column-number-mode t)
;; Display time
(display-time-mode t)
;; Display battery level
(display-battery-mode t)

;; On prog mode, show line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Full screen on init
(add-hook 'after-init-hook 'toggle-frame-fullscreen)

;; Set fringes
(fringe-mode 0) ;; no fringes

;; Fonts
(set-face-attribute 'default
                    nil
                    :family *mono-font*
                    :height (* *font-size* 10))

(set-face-attribute 'fixed-pitch
                    nil
                    :family *mono-font*
                    :height (* *font-size* 10))

(set-face-attribute 'variable-pitch
                    nil
                    :family *mono-font*
                    :height (* *font-size* 12))


;; Use ibuffer to list buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Filter groups
(setq ibuffer-saved-filter-groups
      '(("default"
         ("buffers" (or (name . "\*dashboard\*")
                        (name . "\*scratch\*")))
         ("magit" (name . "magit*"))
         ("elevo" (filename . "Code/elevo-rails"))
         ("emacs" (filename . ".emacs.d"))
         ("org" (mode . org-mode))
         ("dired" (mode . dired-mode))
         ("code" (filename . "Code"))
         ("manuals" (mode . man-mode)))))

;; Add hook
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-switch-to-saved-filter-groups "default")))

;; Do not show groups that are empty
(setq ibuffer-show-empty-filter-groups nil)
;; Do not prompt when deleting a new buffer
(setq ibuffer-expert t)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.2
        which-key-idle-secondary-delay 0.2))

(use-package treemacs
  :defer t
  :config (setq treemacs-no-png-images t)
  :bind (:map global-map
              ("C-x t t" . treemacs)))

(use-package olivetti
  :defer t
  :config (setq olivetti-body-width 110))

(use-package page-break-lines
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode))


;; Bookmarks (this should live elsewhere)

(defun bookmark-update (bookmark)
  "Choose a BOOKMARK from list and change its location."
  (interactive
   (list
    (completing-read
     "Current Bookmarks: "
     (mapcar #'car bookmark-alist))))
  (bookmark-set bookmark)
  (message "The bookmark %s has been updated" bookmark))

(define-key global-map (kbd "C-x r M") 'bookmark-update)

(provide 'ui)

;;; ui.el ends here
