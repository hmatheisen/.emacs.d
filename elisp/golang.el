;;; golang.el --- Golang

;;; Commentary:
;;; Golang settings

;;; Code:

(defun my-go-mode-hook ()
  "Functions for the go-mode-hook."
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-mode
  :ensure t
  :init
  (setq exec-path (cons "/usr/local/go/bin" exec-path))
  (add-to-list 'exec-path "~/go/bin")
  :hook (go-mode . my-go-mode-hook))

;;; golang.el ends here
