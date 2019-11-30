;;; keybindings.el --- My Key Bindings

;;; Commentary:
;;; My Emacs Key Bindings

;;; Code:

;; Avoid MacOS Troubles
(setq mac-option-modifier nil
      mac-command-modifier 'meta
	  select-enable-clipboard t)

;; Resize frames
(global-set-key (kbd "M--") 'shrink-window)
(global-set-key (kbd "M-+") 'enlarge-window)
(global-set-key (kbd "C--") 'shrink-window-horizontally)
(global-set-key (kbd "C-+") 'enlarge-window-horizontally)

;; Undo
;; TODO : Try undo tree
(global-set-key (kbd "C-z") 'undo)

;; Switch buffers
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)

;; Easier keymap for other window
(global-set-key (kbd "M-o") 'other-window)

;; Move back tp previous window
(global-set-key (kbd "M-O") '(lambda ()
							   (interactive)
							   (other-window -1)))

;; New Term Script
(require 'term)
(use-package new-term
  :init
  (global-set-key (kbd "C-x t") 'toggle-term-window)
  (define-key term-raw-map (kbd "C-c <up>") 'bigger-term-window)
  (define-key term-raw-map (kbd "C-c <down>") 'smaller-term-window)
  (define-key term-raw-map (kbd "C-c q") 'quit-term))

;; Move Text
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

;; Split window AZERTY
(global-set-key (kbd "C-x &") 'delete-other-windows)
(global-set-key (kbd "C-x é") 'split-window-vertically)
(global-set-key (kbd "C-x \"") 'split-window-horizontally)
(global-set-key (kbd "C-x à") 'delete-window)

;;; keybindings.el ends here
