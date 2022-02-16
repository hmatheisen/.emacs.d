(use-package yaml-mode
  :defer t)

(use-package tex
  :defer t
  :diminish auto-fill-function
  :ensure auctex
  :config
  ;; Disable auto locale
  (setq TeX-auto-local nil)
  ;; Set TEXINPUTS to recognize classes in custom directory on MacOS
  (when *is-a-mac*
    (setenv "TEXINPUTS" (concat (getenv "TEXINPUTS")
                                ":$HOME/Documents/Notes/classes")))
  :hook (LaTeX-mode . (lambda () (auto-fill-mode 1)
                        (set-fill-column 80))))

(use-package ruby-electric
  :diminish ruby-electric-mode
  :defer t
  :hook (ruby-mode . ruby-electric-mode))
