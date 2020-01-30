;;; org-mode.el --- Org Settings
;;; Commentary:
;;; My settings for Org Mode

;;; Code:

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)))

(use-package org-bullets :hook (org-mode . org-bullets-mode))

(provide 'org-mode)
;;; org-mode.el ends here
