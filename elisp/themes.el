;;; themes.el --- My Themes

;;; Commentary:
;;; Themes I like

;;; Code:

;; Spacemacs Theme
(use-package spacemacs-common
  :ensure spacemacs-theme)
;; Moe Theme
(use-package moe-theme)

;; Use theme-switcher
(use-package theme-switcher
  :ensure nil
  :init
  (setq light-theme 'spacemacs-light)
  (setq dark-theme 'spacemacs-dark))

(provide 'themes)

;;; themes.el ends here
