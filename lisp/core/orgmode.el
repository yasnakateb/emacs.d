;;; latest org
(use-package org
  :ensure t
  :defer t
  :mode ("\\.org$" . org-mode)
  :init
  (setq org-directory     "~/Dropbox/org")
  ;;open agenda in current window
  (setq org-agenda-window-setup (quote current-window))
  ;;warn me of any deadlines in next 7 days
  (setq org-deadline-warning-days 7)
  ;;show me tasks scheduled or due in next fortnight
  (setq org-agenda-span (quote fortnight))
  ;;don't show tasks as scheduled if they are already shown as a deadline
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  ;;don't give awarning colour to tasks with impending deadlines
  ;;if they are scheduled to be done
  (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
  ;;don't show tasks that are scheduled or have deadlines in the
  ;;normal todo list
  (setq org-agenda-todo-ignore-deadlines (quote all))
  (setq org-agenda-todo-ignore-scheduled (quote all))
  ;;sort tasks in order of when they are due and then by priority
  (setq org-agenda-sorting-strategy
	(quote
	 ((agenda deadline-up priority-down)
	  (todo priority-down category-keep)
	  (tags priority-down category-keep)
	  (search category-keep))))
  ;; capture
  (setq org-default-notes-file (concat org-directory "/agenda/notes.org"))
  (setq org-capture-templates
	'(("t" "todo" entry (file+headline org-default-notes-file "Tasks")
	   "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))
  :config
  (require 'org-id))
;;; org-bullets
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;;; org-reveal
(use-package ox-reveal
:ensure ox-reveal)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)
(use-package htmlize
:ensure t)
;;; gnuplot
(use-package gnuplot
  :ensure t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)))
(defun insert-file-as-org-table (filename)
  "Insert a file into the current buffer at point, and convert it to an org table."
  (interactive (list (ido-read-file-name "csv file: ")))
  (let* ((start (point))
    (end (+ start (nth 1 (insert-file-contents filename)))))
    (org-table-convert-region start end))) 
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
