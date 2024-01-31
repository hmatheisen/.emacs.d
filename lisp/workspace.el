;;; workspace.el --- Create emacs frames and windows  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Henry MATHEISEN

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

(require 'magit)
(require 'cl-lib)

(defvar workspace-config nil
  "Config list for frames and windows.")

(defun workspace-create ()
  "Create a new workspace."
  (interactive)
  (unless workspace-config
    (error "No worspace config defined"))
  (dolist (frame-config workspace-config)
    (let ((frame (if (eq frame-config (car workspace-config))
                     (selected-frame)
                   (make-frame)))
          (window-actions (cdr frame-config))
          (path (car frame-config)))
      (with-selected-frame frame
        (if (length> window-actions 3)
            (error "Can't handle more than 3 actions per frame"))
        (dolist (action window-actions)
          (let* ((position (cl-position action window-actions))
                 (window (cond ((= position 0) (selected-window))
                               ((= position 1) (split-window-right nil (car (window-list))))
                               ((= position 2) (split-window-below nil (nth 1 (window-list)))))))
            (with-selected-window window
              (cond ((eq action 'dired) (dired path))
                    ((eq action 'magit) (magit-status path))
                    ((listp action) (eval action))
                    ((stringp action)
                     (find-file (concat path action)))))))))))

(provide 'workspace)

;;; workspace.el ends here
