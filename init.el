(setq inhibit-startup-message t)
(tool-bar-mode -1)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
	     :ensure t)

(org-babel-load-file (expand-file-name "~/.emacs.d/configs/org_mode.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/buffer.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/window.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/completion.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/themes.org"))

