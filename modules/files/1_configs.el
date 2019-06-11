;;; configs.el --- Files -*- lexical-binding: t; -*-

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

;;; reuse buffer
(require 'dired-single)
(defun my-dired-init ()
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse))
(if (boundp 'dired-mode-map)
    (my-dired-init)
  (add-hook 'dired-load-hook 'my-dired-init))

;;; using ls-lisp with these settings gives case-insensitve
(require 'ls-lisp)
(setq dired-listing-switches "-lhG"
      ls-lisp-use-insert-directory-program nil
      ls-lisp-ignore-case t
      ls-lisp-use-string-collate nil
      ls-lisp-verbosity '(links uid)
      ls-lisp-format-time-list '("%b %e %H:%M" "%b %e  %Y")
      ls-lisp-use-localized-time-format t)

;;; allow editing file permissions
(setq wdired-allow-to-change-permissions t)

;;; dired-quick-sort
(with-eval-after-load 'dired-quick-sort
  (dired-quick-sort-setup))

;;; neotree
(with-eval-after-load 'neotree
  (setq projectile-switch-project-action 'neotree-projectile-action
	neo-smart-open t
	neo-theme (if (display-graphic-p) 'icons 'arrow)))


;;; configs.el ends here
