;;; ivy-counsel.el --- Ivy Mode

;;; Commentary:
;;; Settings for ivy, counsel and swiper which are really cool

;;; Code:

(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (("C-s" . swiper-isearch))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(provide 'ivy-counsel)

;;; ivy-counsel.el ends here
