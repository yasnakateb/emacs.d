;;; latest org
(use-package org
  :ensure t
  :defer t
  :mode ("\\.org$" . org-mode)
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
