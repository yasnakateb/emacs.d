;;; ibuffer interactive mode
;; set it as default
(defalias 'list-buffers 'ibuffer-other-window)
;; better look
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
  (quote (("default"
    ("dired" (mode . dired-mode))
    ("org" (name . "^.*org$"))
    
    ("web" (or (mode . web-mode) (mode . js2-mode)))
    ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
    ("mu4e" (name . "\*mu4e\*"))
    ("programming" (or
      (mode . python-mode)
      (mode . c++-mode)))
    ("emacs" (or
    (name . "^\\*scratch\\*$")
    (name . "^\\*Messages\\*$")))
    ))))
(add-hook 'ibuffer-mode-hook
   (lambda ()
   (ibuffer-auto-mode 1)
   (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)
;;; avy
(use-package avy
    :ensure t
    :config
    (avy-setup-default)
    :bind ("M-s" . avy-goto-char))

;;; undo-tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))
;;; hungry-delete mode
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))
;;; expand-region mode
(use-package expand-region
  :ensure t
  :config 
  (global-set-key (kbd "C-=") 'er/expand-region))
;;; y-n-p
(fset 'yes-or-no-p 'y-or-n-p)
;;; projectile/counsel-projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm))
;;; nlinum-relative
(use-package nlinum-relative
  :ensure t
  :config
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (setq nlinum-relative-redisplay-delay 0)
  (setq nlinum-relative-current-symbol "")
  (setq nlinum-relative-offset 1))
;;; emojify
(use-package emojify
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-emojify-mode))
;;; google-this
(use-package google-this
  :ensure t
  :init
  (google-this-mode 1))
;;; FIXMEe
(use-package fixmee
  :ensure t
  :after button-lock
  :init
  (global-fixmee-mode 1))
;;; dark room
(use-package darkroom
  :ensure t
  :defer t)
;;; defaults
;; browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")
;;kill buffer
(setq kill-buffer-query-functions 
  (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
;; man
(require 'man)
(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)
