;;; company mode
(use-package company
  :ensure t
  :init  
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
  (setq company-idle-delay 0))
;;; auto-complete mode
(use-package auto-complete
  :ensure t
  :init
    (ac-config-default)
    (global-auto-complete-mode t)
    )
;;; flycheck mode
(use-package flycheck
  :defer t
  :ensure t)
;;; yasnippet mode
(use-package yasnippet
  :ensure t
  :defer t)
;;; yasnippet-snippets
(use-package yasnippet-snippets
  :defer t
  :after yasnippet
  :ensure t)
