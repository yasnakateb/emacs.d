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

;;; swiper-mode/counsel
(use-package counsel
  :ensure t
  )
(use-package swiper
  :ensure try
  :init
  (ivy-mode 1)
  :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))
    (setq ivy-initial-inputs-alist nil)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    )
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
  (setq projectile-completion-system 'ivy))
(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))
;;; nlinum-relative
(use-package nlinum-relative
  :ensure t
  :config
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (setq nlinum-relative-redisplay-delay 0)
  (setq nlinum-relative-current-symbol "")
  (setq nlinum-relative-offset 1))
;;; backups
(let ((backup-dir "~/.emacs.d/backups")
      (auto-saves-dir "~/.emacs.d/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))
(setq backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 5
      kept-old-versions 2)
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
