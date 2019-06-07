;;; init.el --- skeleton config  -*- lexical-binding: t; coding:utf-8; fill-column: 119 -*-

;;; Commentary:
;; A bare-boned config template. Use "outshine-cycle-buffer" (<Tab> and <S-Tab>
;; in org style) to navigate through sections, and "imenu" to locate individual
;; use-package definition.

;;; Bootstrap
;; Speed up startup

(eval-when-compile (require 'cl))

(lexical-let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

(let ((gc-cons-threshold (* 256 1024 1024))
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil)
      (core-directory (concat user-emacs-directory "core/"))
      (bindings-directory (concat user-emacs-directory "bindings/"))
      (config-directory (concat user-emacs-directory "config/"))))
;; Initialize package.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(setq-default use-package-always-ensure t ; Auto-download package if not exists
              use-package-always-defer t ; Always defer load package to speed up startup
              use-package-verbose nil ; Don't report loading details
              use-package-expand-minimally t  ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Add system-wide defaults here, for example:
;;
;; (setq-default inhibit-startup-message t
;;               initial-scratch-message nil)

;; Add all use-package definitions from here
(use-package try
  :ensure t)
(setq custom-file "~/.emacs.d/backups/custom.el")
(load custom-file 'noerror)

(use-package outshine
  ;; Easier navigation for source files, especially this one.
  :bind (:map outshine-mode-map
    ("<S-iso-lefttab>" . outshine-cycle-buffer))
  :hook (emacs-lisp-mode . outshine-mode))
;; When config gets stable, using emacs server may be more convenient
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))
;;; evil
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-default-cursor t)
  ;; This has to be before we invoke evil-mode due to:
  ;; https://github.com/cofi/evil-leader/issues/10
  (use-package evil-leader
      :init (global-evil-leader-mode))
  (evil-mode 1))
  
;;; init.el ends here
