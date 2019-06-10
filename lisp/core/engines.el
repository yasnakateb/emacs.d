(use-package helm
  :ensure t
  :init
  (helm-mode 1)
  :config
  (add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.35)))
  (define-key helm-find-files-map "\t" 'helm-execute-persistent-action))
