;;; Shift navigation
(windmove-default-keybindings)
;;; ace-window
(use-package ace-window
  :ensure t
  :init
    (global-set-key [remap other-window] 'ace-window))
;;; winner-mode
(winner-mode 1)
;;; all-the-icons
(use-package all-the-icons
  :ensure t)
;;; imenu
(use-package imenu
  :ensure t)
;;; imenu-list
(use-package imenu-list
    :ensure t
    :init
      (setq imenu-list-focus-after-activation t
            imenu-list-auto-resize t)
    :config
    (global-set-key (kbd "C-c i") #'imenu-list-smart-toggle))
;;; neo-tree
(use-package neotree
  :ensure t
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-smart-open t)
  (global-set-key (kbd "C-c f t") 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
;;; zoom
(use-package zoom
  :ensure t
  :init
  (zoom-mode t)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))))
