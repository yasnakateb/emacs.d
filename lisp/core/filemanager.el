;;; reuse buffer
(require 'dired-single)
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse))
(if (boundp 'dired-mode-map)
        (my-dired-init)
  (add-hook 'dired-load-hook 'my-dired-init))
;;; using ls-lisp with these settings gives case-insensitve
(require 'ls-lisp)
(setq dired-listing-switches "-lhG")
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-ignore-case t)
(setq ls-lisp-use-string-collate nil)
;;; customise the appearance of the listing
(setq ls-lisp-verbosity '(links uid))
(setq ls-lisp-format-time-list '("%b %e %H:%M" "%b %e  %Y"))
(setq ls-lisp-use-localized-time-format t)
;; allow editing file permissions
(setq wdired-allow-to-change-permissions t)
;;; preview files in dired
(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :init
  (setq peep-dired-cleanup-on-disable t)
;  (setq peep-dired-cleanup-eagerly t)
  (setq peep-dired-enable-on-directories t)
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4")))
;;narrow dired to match filter
(use-package dired-narrow
  :ensure t)
;;; dired subtree
(use-package dired-subtree
  :ensure t)
;;; dired-quick-sort
(use-package dired-quick-sort
  :ensure t
  :config
  (dired-quick-sort-setup))
;;; neo-tree
(use-package neotree
  :ensure t
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-smart-open t)
  (global-set-key (kbd "C-c f t") 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
