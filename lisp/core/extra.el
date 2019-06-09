;;; outshine
(use-package outshine
  ;; Easier navigation for source files, especially this one.
  :bind (:map outshine-mode-map
    ("<S-iso-lefttab>" . outshine-cycle-buffer))
  :hook (emacs-lisp-mode . outshine-mode))
;;; try
(use-package try
  :ensure t)
;; smartparens
(use-package smartparens
  :ensure t)
;;; zoom
(use-package zoom
  :ensure t
  :init
  (zoom-mode t)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))))
;;; dark room
(use-package darkroom
  :ensure t
  :defer t)
