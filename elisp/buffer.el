;;; buffer.el --- iBuffer

;;; Commentary:
;;; iBuffer settings

;;; Code:

;; Use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Create home filter group
(setq ibuffer-saved-filter-groups
      '(("home"
         ("emacs-config" (filename . ".emacs.d"))
         ("Terminal" (mode . term-mode))
         ("Magit" (mode . magit-mode))
         ("Org" (or (mode . org-mode)
                    (filename . "Org")
                    (name . "\*Org Agenda\*")))
         ("Help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*"))))))

;; Add filter group
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-switch-to-saved-filter-groups "home")))

(provide 'buffer)
;;; buffer.el ends here
