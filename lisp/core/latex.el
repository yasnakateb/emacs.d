;;; auctex
(use-package auctex
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
	      ;(TeX-source-correlate-mode)
              (turn-on-reftex)
              (reftex-isearch-minor-mode)
	      (add-to-list 'TeX-view-program-selection
			   '(output-pdf "Zathura"))
	      (setq-default TeX-master nil)
              (setq reftex-plug-into-AUCTeX t
                    TeX-PDF-mode t
		    TeX-command-force ""
		    ;TeX-source-correlate-method 'synctex
		    ;TeX-source-correlate-start-server t
		    ))))
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
		     (TeX-command-master arg))))))
  ;; to use pdfview with auctex
;;; reftex
(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)); Prompt for empty optional arguments in cite
;;; pdf-tools
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :bind ("C-c C-g" . pdf-sync-forward-search)
  :defer t
  :config
  (setq mouse-wheel-follow-mouse t)
  (setq pdf-view-resize-factor 1.10))
;;; helm-bibtex
(use-package helm-bibtex
  :defer t
  :ensure t
  :config

  (setq bibtex-completion-bibliography "~/Dropbox/org/ref/master.bib"
        bibtex-completion-library-path "~/Dropbox/org/ref/pdfs"
        bibtex-completion-notes-path   "~/Dropbox/org/ref/notes.org")
  (setq helm-bibtex-bibliography '("~/Dropbox/org/ref/master.bib"))
  (setq helm-bibtex-library-path '("~/Dropbox/org/ref/pdfs"))
  ;; using bibtex path reference to pdf file
  (setq bibtex-completion-pdf-field "File")
  (setq helm-bibtex-default-action 'bibtex-completion-insert-citation))

;;; auctex-latexmk
(use-package auctex-latexmk
  :ensure t
  :defer t
  :config
  (auctex-latexmk-setup)
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))
