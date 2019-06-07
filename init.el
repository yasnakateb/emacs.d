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
      (file-name-handler-alist nil)))
;; Initialize package.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq custom-file "~/.emacs.d/backups/custom.el")
(load custom-file 'noerror)

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

(defun load-directory (dir)
      (let ((load-it (lambda (f)
		       (load-file (concat (file-name-as-directory dir) f)))
		     ))
	(mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/ui")
(load-directory "~/.emacs.d/core")
(load-directory "~/.emacs.d/bindings")
;;; init.el ends here
