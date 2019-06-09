;;; hydra
(use-package hydra
  :ensure t)
;;; general
(use-package general
  :ensure t
  :config
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (general-override-mode))
;;; which-key
(use-package which-key
  :ensure t
  :init
  (which-key-mode))
