;;; magit
(use-package magit
  :ensure t
  :init
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  :config
  (global-set-key (kbd "C-c g") 'magit-status))
;;; git-gutter
(use-package git-gutter-fringe+
  :ensure t
  :config
  (progn
  (global-git-gutter+-mode t)
  (custom-set-variables
  '(git-gutter:update-interval 2))
  (set-face-foreground 'git-gutter-fr+-modified "purple")
  (set-face-foreground 'git-gutter-fr+-added "green")
  (set-face-foreground 'git-gutter-fr+-deleted "red")
  (setq git-gutter-fr+-side 'right-fringe)
  (setq-default left-fringe-width  20)
  (setq-default right-fringe-width 10)

  (fringe-helper-define 'git-gutter-fr+-added nil
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX....."
  ".XX.....")

  (fringe-helper-define 'git-gutter-fr+-deleted nil
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX.")
  
  (fringe-helper-define 'git-gutter-fr+-modified nil
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX..."
  ".XXXX...")))
