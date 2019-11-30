;;; markdown.el --- Markdown

;;; Commentary:
;;; Markdown settings

;;; Code:

(defun my-markdown-mode-hook ()
  "Markdown/GFM mode hook."
  (toggle-truncate-lines -1)
  (toggle-word-wrap t)
  (linum-mode -1)
  (flyspell-mode)
  (auto-fill-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "/usr/local/bin/multimarkdown")
  (setq markdown-fontify-code-blocks-natively t)
  (add-hook 'markdown-mode-hook 'my-markdown-mode-hook)
  (add-hook 'gfm-mode-hook 'my-markdown-mode-hook))

;;; markdown.el ends here
