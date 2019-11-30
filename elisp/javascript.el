;;; javascript.el --- Javascript/Typescript

;;; Commentary:
;;; JS/TS settings

;;; Code:

(use-package js2-mode
  :ensure t
  :interpreter (("node" . js2-mode))
  :bind (:map js2-mode-map ("C-c C-p" . js2-print-json-path))
  :mode "\\.\\(js\\|json\\)$"
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (setq js2-basic-offset 2
        js2-highlight-level 3
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

(use-package tern
  :ensure t
  :config
  (progn
    (add-hook 'js-mode-hook (lambda () (tern-mode t)))
    (add-hook 'js2-mode-hook (lambda () (tern-mode t)))))

(use-package company-tern
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-tern))

;;; javascript.el ends here
