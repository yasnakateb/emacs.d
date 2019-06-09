;;; Shift navigation
(windmove-default-keybindings)
;;; ace-window
(use-package ace-window
  :ensure t
  :init
    (global-set-key [remap other-window] 'ace-window))
;;; winner-mode
(winner-mode 1)
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
