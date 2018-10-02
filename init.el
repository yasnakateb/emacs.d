(setq inhibit-startup-message t)
(tool-bar-mode -1)

(require 'package)
;;(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
	     :ensure t)
(setq custom-file "~/.emacs.d/backups/custom.el")
(load custom-file 'noerror)
(load-library "~/.emacs.d/secrets/passwords.el.gpg")
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/themes.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/buffer.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/window.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/org_mode.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/completion.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/python_mode.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/cc_mode.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/vcs.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/latex.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/mail.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/key.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/shell.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/social.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/web.org"))

(provide 'init)
