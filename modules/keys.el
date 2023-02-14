(global-set-key (kbd "<escape>") 'keyboard-quit)

(use-package evil
  :init
  ;; C-u is Vi behaviour
  (setq evil-want-C-u-scroll t)
  :config
  ;; Only start evil in prog-mode
  ;; (add-hook 'prog-mode-hook 'evil-local-mode)
  ;; Customize undo system
  (evil-set-undo-system 'undo-tree)
  ;; Use SPC instead of : for commands
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") 'evil-ex)))
