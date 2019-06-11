;;; packages.el --- Org -*- lexical-binding: t; -*-

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

;;; latest org
(use-package org
  :ensure t
  :mode ("\\.org$" . org-mode)
  :init
  (setq org-directory     "~/Dropbox/org"
	org-agenda-files
	(append
	 (file-expand-wildcards (concat org-directory "/agenda/*.org")))
	org-agenda-window-setup (quote current-window)
	org-deadline-warning-days 7
	org-agenda-span (quote fortnight)
	org-agenda-skip-scheduled-if-deadline-is-shown t
	org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
	org-agenda-todo-ignore-deadlines (quote all)
	org-agenda-todo-ignore-scheduled (quote all)
	org-agenda-sorting-strategy (quote
				     ((agenda deadline-up priority-down)
				      (todo priority-down category-keep)
				      (tags priority-down category-keep)
				      (search category-keep)))
	org-default-notes-file (concat org-directory "/agenda/notes.org")
	org-capture-templates
	'(("t" "todo" entry (file+headline org-default-notes-file "Tasks")
	   "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"))))

;;; org-bullets
(use-package org-bullets
  :ensure t)

;;; org-reveal
(use-package ox-reveal
  :ensure ox-reveal)

(use-package htmlize
:ensure t)

;;; gnuplot
(use-package gnuplot
  :ensure t)

;;; org-ref
(use-package org-ref
  :ensure t
  :defer t
  :init
  (setq org-ref-bibliography-notes     (concat org-directory "/ref/notes.org")
        org-ref-default-bibliography   '(concat org-directory "/ref/master.bib")
        org-ref-pdf-directory          (concat org-directory "/ref/pdfs/"))
  (setq org-latex-pdf-process '("latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))
  (setq interleave-org-notes-dir-list `(,(concat org-directory "/ref/pdfs"))))

;;; packages.el ends here
