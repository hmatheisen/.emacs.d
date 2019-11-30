;;; company-mode.el --- Company

;;; Commentary:
;;; My settings for Company Mode

;;; Code:

(use-package company-mode
  :init
  (setq company-selection-wrap-around t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (with-eval-after-load 'company
	(define-key company-active-map (kbd "M-n") nil)
	(define-key company-active-map (kbd "M-p") nil)
	(define-key company-active-map (kbd "C-n") 'company-select-next)
	(define-key company-active-map (kbd "C-p") 'company-select-previous))
  :ensure company
  :hook (after-init . global-company-mode))

;;; company-mode.el ends here
