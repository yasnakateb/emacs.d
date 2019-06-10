;;; init.el --- init file -*- lexical-binding: t; -*-

;; Copyright (C) 2019  M.R. Siavash Katebzadeh

;; Author: M.R.Siavash Katebzadeh <mr.katebzadeh@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A bare-boned config template. Use "outshine-cycle-buffer" (<Tab> and <S-Tab>
;; in org style) to navigate through sections, and "imenu" to locate individual
;; use-package definition.

;;; Speed up startup

(eval-when-compile (require 'cl))

(lexical-let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

(let ((gc-cons-threshold (* 256 1024 1024))
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil)))
;;; Basic configs
; stop creating backup~ files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(let ((backup-dir "~/.emacs.d/backups")
      (auto-saves-dir "~/.emacs.d/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))
(setq backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 5
      kept-old-versions 2)
(setq custom-file "~/.emacs.d/backups/custom.el")
(load custom-file 'noerror)

;;; Load directory function
(defun load-directory (dir)
      (let ((load-it (lambda (f)
		       (load-file (concat (file-name-as-directory dir) f)))
		     ))
	(mapc load-it (directory-files dir nil "\\.el$"))))
;;; Load core
(load-directory "~/.emacs.d/core")
;;; Load modules
(load-directory "~/.emacs.d/modules/buffers/")
(load-directory "~/.emacs.d/modules/completion/")
(load-directory "~/.emacs.d/modules/engines/")
(load-directory "~/.emacs.d/modules/evil/")
(load-directory "~/.emacs.d/modules/extra/")
(load-directory "~/.emacs.d/modules/files/")
(load-directory "~/.emacs.d/modules/git/")
(load-directory "~/.emacs.d/modules/latex/")
(load-directory "~/.emacs.d/modules/mail/")
(load-directory "~/.emacs.d/modules/org/")
(load-directory "~/.emacs.d/modules/python/")
(load-directory "~/.emacs.d/modules/rss/")
(load-directory "~/.emacs.d/modules/shell/")
(load-directory "~/.emacs.d/modules/social/")
(load-directory "~/.emacs.d/modules/web/")
(load-directory "~/.emacs.d/modules/window/")
(provide 'init)
;;; init.el ends here
