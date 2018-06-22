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
(org-babel-load-file (expand-file-name "~/.emacs.d/configs/python_mode.org"))
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (hungry-delete beacon yasnippet-snippets yasnippet jedi which-key use-package try ox-reveal org-bullets htmlize flycheck dracula-theme counsel auto-complete ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
