;;; lang.el --- Packages for unsupported languages   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Henry MATHEISEN

;; Author: Henry MATHEISEN <haineriz@posteo.de>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(use-package yaml-mode
  :defer t)

(use-package ruby-electric
  :diminish ruby-electric-mode
  :defer t
  :hook (ruby-mode . ruby-electric-mode))

;; Shell settings
(setq sh-basic-offset 2)

;; Typescript
(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))

(use-package web-mode
  :mode (("\\.tsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; JS indent
(setq js-indent-level 2)

(provide 'lang)
;;; lang.el ends here
