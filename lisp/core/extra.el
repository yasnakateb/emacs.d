;;; outshine
(use-package outshine
  ;; Easier navigation for source files, especially this one.
  :bind (:map outshine-mode-map
    ("<S-iso-lefttab>" . outshine-cycle-buffer))
  :hook (emacs-lisp-mode . outshine-mode))
;;; try
(use-package try
  :ensure t)
