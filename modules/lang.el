(use-package yaml-mode
  :defer t)

(use-package ruby-electric
  :diminish ruby-electric-mode
  :defer t
  :hook (ruby-mode . ruby-electric-mode))
