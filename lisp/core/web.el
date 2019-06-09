;;; web-mode
(use-package web-mode
  :ensure t
  :defer t)
;;; company-web-html
(use-package company-web
  :ensure t
  :defer t
  :config
  (add-hook 'web-mode-hook
       (lambda ()
          (set (make-local-variable 'company-backends) '(company-web-html))
            (company-mode t))))
;;; company settings
(setq company-tooltip-limit 20)
(setq company-tooltip-align-annotations 't)
(setq company-idle-delay .3)
(setq company-begin-commands '(self-insert-command))
;;; react native
(use-package js2-mode
  :ensure t
  :defer t)
(use-package nodejs-repl
  :ensure t
  :defer t)
(use-package tide
  :ensure t
  :defer t)
(setq js2-basic-offset 2
      js2-bounce-indent-p t)

(add-hook 'js2-mode-hook 'company-mode)

(add-hook 'js2-mode-hook
          (lambda ()
            (define-key js2-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-sexp)
            (define-key js2-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
            (define-key js2-mode-map (kbd "C-c C-b") 'nodejs-repl-send-buffer)
            (define-key js2-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
            (define-key js2-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setq company-tooltip-align-annotations t)

(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
