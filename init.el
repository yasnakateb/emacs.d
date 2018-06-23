(setq inhibit-startup-message t)
(tool-bar-mode -1)

(require 'package)
;;(setq package-enable-at-startup nil)
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

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dashboard hydra general evil-magit evil-collection evil gnus-dired mu4e-alert mu4e cmake-mode cmake-project yasnippet-snippets which-key use-package undo-tree try rainbow-delimiters ox-reveal org-bullets neotree magit jedi irony-eldoc imenu-list hungry-delete htmlize helm-rtags google-c-style ggtags flycheck-rtags flycheck-irony expand-region dumb-jump dracula-theme counsel-projectile company-rtags company-irony beacon autopair auctex all-the-icons ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
