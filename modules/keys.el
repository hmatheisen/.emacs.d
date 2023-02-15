(global-set-key (kbd "<escape>") 'keyboard-quit)

(use-package evil
  :init
  ;; C-u is Vi behaviour
  (setq evil-want-C-u-scroll t)
  ;; Default mode is emacs
  (setq evil-default-state 'emacs)
  ;; Override list of buffers where default state is something else than "emacs"
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)
  (setq evil-normal-state-modes nil)
  :config
  ;; Customize undo system
  (evil-set-undo-system 'undo-tree)
  ;; Use SPC instead of : for commands
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") 'evil-ex))
  ;; Start evil mode
  (evil-mode 1))
