;;; lsp-conf.el --- Language Server Protocol

;;; Commentary:
;;; awesome lsp mode for ide like features

;;; Code:

(use-package lsp-mode
  :ensure  t
  :init
  (setq lsp-auto-guess-root t)
  :hook
  ((go-mode . lsp)
   (js2-mode . lsp)
   (typescript-mode . lsp)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :init (setq lsp-ui-peek-always-show t
              lsp-ui-doc-enable nil))

(use-package company-lsp
  :ensure t
  :after (company lsp-mode)
  :init
  (setq company-lsp-cache-candidates 'auto)
  :commands (company-lsp))

;;; lsp-conf ends here
