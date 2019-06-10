;;; latest org
(use-package org
  :ensure t
  :defer t
  :mode ("\\.org$" . org-mode)
  :init
  (setq org-directory     "~/Dropbox/org")
  (setq org-agenda-files
	(append
	 (file-expand-wildcards (concat org-directory "/agenda/*.org"))))
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
  (setq interleave-org-notes-dir-list `(,(concat org-directory "/ref/pdfs")))
  :config
  (defun my-orcb-key ()
    "Replace the key in the entry, also change the pdf file name if it exites."
    (let ((key (funcall org-ref-clean-bibtex-key-function
			(bibtex-generate-autokey))))
      ;; first we delete the existing key
      (bibtex-beginning-of-entry)
      (re-search-forward bibtex-entry-maybe-empty-head)

      (setq old-key (match-string 2));;store old key
      
      (if (match-beginning bibtex-key-in-head)
	  (delete-region (match-beginning bibtex-key-in-head)
			 (match-end bibtex-key-in-head)))
      ;; check if the key is in the buffer
      (when (save-excursion
	      (bibtex-search-entry key))
	(save-excursion
	  (bibtex-search-entry key)
	  (bibtex-copy-entry-as-kill)
	  (switch-to-buffer-other-window "*duplicate entry*")
	  (bibtex-yank))
	(setq key (bibtex-read-key "Duplicate Key found, edit: " key)))
      (insert key)
      (kill-new key)
      
      (save-excursion
	"update pdf names and notes items"
	;; rename the pdf after change the bib item key
	(my-update-pdf-names old-key key)
	;; renmae the notes item after change the bib item key
	(my-update-notes-item old-key key))
      
      ;; save the buffer
      (setq require-final-newline t)
      (save-buffer)))
  ;; define a function that update the pdf file names before change the key of a bib entry
  (defun my-update-pdf-names (old-key new-key)
    (let ((old-filename (concat org-ref-pdf-directory old-key ".pdf"))
	  (new-filename (concat org-ref-pdf-directory new-key ".pdf" )))
      (if (file-exists-p old-filename)
	  (rename-file old-filename new-filename))))
  ;; define a function that update the notes items before change the key of bib entry
  (defun my-update-notes-item (old-key new-key)
    "update a notes item of a old-key by a new-key in case the bib item is changed"
    
    (set-buffer (find-file-noselect org-ref-bibliography-notes))
    ;; move to the beginning of the buffer
    (goto-char (point-min))
    ;; find the string and replace it
    (let ((newcite new-key)
	  (regstr old-key))
      
      (while (re-search-forward regstr nil t)
	
	(delete-region (match-beginning 0)
		       (match-end 0))
	(insert newcite))
      
      ;; save the buffer
      (setq require-final-newline t)
      (save-buffer)
      (kill-buffer)))
  (add-hook 'org-ref-clean-bibtex-entry-hook 'my-orcb-key))
;;; org-beamer
(with-eval-after-load 'org
  (unless (boundp 'org-export-latex-classes)
    (setq org-export-latex-classes nil))
  (add-to-list 'org-export-latex-classes
	       ;; beamer class, for presentations
	       '("beamer"
		 "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"
		 
		 ("\\section{%s}" . "\\section*{%s}")
		 
		 ("\\begin{frame}[fragile]\\frametitle{%s}"
		  "\\end{frame}"
		  "\\begin{frame}[fragile]\\frametitle{%s}"
		  "\\end{frame}")))
  
  ;; letter class, for formal letters
  
  (add-to-list 'org-export-latex-classes
	       
	       '("letter"
		 "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"
		 
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


  (setq org-latex-create-formula-image-program 'imagemagick)
  (setq org-latex-packages-alist
	(quote (("" "color" t)
		("" "minted" t)
		("" "parskip" t)
		("" "tikz" t)))))

;;; elfeed-org
(use-package elfeed-org
  :after elfeed
  :defer t
  :ensure t
  :config
  (setq rmh-elfeed-org-files  (list (concat org-directory "/feed/emacs.org")
				    (concat org-directory "/feed/research.org"))))
