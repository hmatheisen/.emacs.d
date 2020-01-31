;;; lsp-init.el --- Language Server Protocol

;;; Commentary:
;;; awesome lsp mode for ide like features
;;; company settings to enable completion

;;; Code:

(use-package lsp-mode)

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

(use-package company-lsp
  :after (company lsp)
  :init
  (push 'company-lsp company-backends))

(provide 'lsp-init)
;;; lsp-init ends here
