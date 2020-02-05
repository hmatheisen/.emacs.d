;;; keybindings.el --- My Key Bindings

;;; Commentary:
;;; My Emacs Key Bindings

;;; Code:

(use-package window
  :ensure nil
  :config
  ;; Resizing
  (global-set-key (kbd "M--") 'shrink-window)
  (global-set-key (kbd "M-+") 'enlarge-window)
  (global-set-key (kbd "C--") 'shrink-window-horizontally)
  (global-set-key (kbd "C-+") 'enlarge-window-horizontally)
  ;; Other window
  (global-set-key (kbd "M-o") 'other-window)
  (global-set-key (kbd "M-O") '(lambda ()
                                 (interactive)
                                 (other-window -1))))

;; New Term Script
(use-package new-term
  :preface
  (defun my-new-term-hook ()
    (define-key term-raw-map (kbd "C-c <up>") 'bigger-term-window)
    (define-key term-raw-map (kbd "C-c <down>") 'smaller-term-window)
    (define-key term-raw-map (kbd "C-c q") 'quit-term))
  :ensure nil
  :init
  (setq new-shell "/usr/local/bin/bash")
  (global-set-key (kbd "C-x t") 'toggle-term-window)
  (add-hook 'term-mode-hook 'my-new-term-hook))

(provide 'keybindings)

;;; keybindings.el ends here
