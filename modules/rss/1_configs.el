;;; configs.el --- Elfeed -*- lexical-binding: t; -*-

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


;;; elfeed-org
(with-eval-after-load 'elfeed-org
  (setq rmh-elfeed-org-files  (list (concat org-directory "/feed/emacs.org")
				    (concat org-directory "/feed/research.org"))))
 
;;; helper functions
(with-eval-after-load 'elfeed
  ;; add a star
  (defun elfeed-star ()
    "Apply starred to all selected entries."
    (interactive )
    (let* ((entries (elfeed-search-selected))
	   (tag (intern "starred")))
      (cl-loop for entry in entries do (elfeed-tag entry tag))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))
  ;; remove a start
  (defun elfeed-unstar ()
    "Remove starred tag from all selected entries."
    (interactive )
    (let* ((entries (elfeed-search-selected))
	   (tag (intern "starred")))
      (cl-loop for entry in entries do (elfeed-untag entry tag))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))
  ;; face for starred articles
  (defface elfeed-search-starred-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")
  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)
  ;;shortcut to jump to starred bookmark
  (defun elfeed-show-starred ()
    (interactive)
    (bookmark-jump "elfeed-starred"))
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'starred))
  ;;searches
  (defun elfeed-show-all ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-all"))
  (defun elfeed-show-emacs ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-emacs"))
  (defun elfeed-show-daily ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-daily"))
  (defun elfeed-show-network ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-network"))
  ;; makes sure elfeed reads index from disk before launching
  (defun elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))
  ;;write to disk when quiting
  (defun elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window)))

;;; configs.el ends here
