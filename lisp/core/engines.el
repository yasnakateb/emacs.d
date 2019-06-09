(use-package helm
  :ensure t
  :init
  (helm-mode 1)
  :config
  (define-key helm-find-files-map "\t" 'helm-execute-persistent-action))
