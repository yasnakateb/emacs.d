
;;; configs.el --- Latex -*- lexical-binding: t; -*-

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

;;; auctex
(with-eval-after-load 'auctex
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (add-hook 'after-save-hook 'TeX-command-master nil t)))
					; to use pdfview with auctex
  (add-hook 'LaTeX-mode-hook 'pdf-tools-install)

  ;; 3 Make C-c-c not ask, just do the default action. Add C-c-a for asking
  ;; If non-nil, TeX-command-query will return the value of this variable instead
  ;; of quering the user.

  (add-hook 'LaTeX-mode-hook
	    '(lambda()
	       (define-key LaTeX-mode-map "\C-c\C-a" ; 'a' for ask
		 (lambda (arg) (interactive "P")
		   (let ((TeX-command-force nil))
		     (TeX-command-master arg)))))))

;;; reftex
(with-eval-after-load 'reftex
  (setq reftex-cite-prompt-optional-args t))

;;; pdf-tools
(with-eval-after-load 'pdf-tools
  (setq mouse-wheel-follow-mouse t
	pdf-view-resize-factor 1.10))

;;; helm-bibtex
(with-eval-after-load 'helm-bibtex
  (setq bibtex-completion-bibliography "~/Dropbox/org/ref/master.bib"
        bibtex-completion-library-path "~/Dropbox/org/ref/pdfs"
        bibtex-completion-notes-path   "~/Dropbox/org/ref/notes.org"
	helm-bibtex-bibliography '("~/Dropbox/org/ref/master.bib")
	helm-bibtex-library-path '("~/Dropbox/org/ref/pdfs")
	;; using bibtex path reference to pdf file
	bibtex-completion-pdf-field "File"
	helm-bibtex-default-action 'bibtex-completion-insert-citation))

;;; auctex-latexmk
(with-eval-after-load 'auctex-latexmk
  (auctex-latexmk-setup))

;;; company-auctex
(with-eval-after-load 'company-auctex
  (company-auctex-init))

;;; configs.el ends here
