(use-package yaml-mode
  :defer t)

(use-package ruby-electric
  :diminish ruby-electric-mode
  :defer t
  :hook (ruby-mode . ruby-electric-mode))

;; Shell settings
(setq sh-basic-offset 2)
