;;; evil
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-default-cursor t)
  (setq evil-want-integration nil)
  (setq evil-want-keybinding nil)
  ;; This has to be before we invoke evil-mode due to:
  ;; https://github.com/cofi/evil-leader/issues/10
  (use-package evil-leader
      :init (global-evil-leader-mode))
  (evil-mode 1))
;;; evil-org
(use-package evil-org
  :after evil
  :ensure t)
;;; evil-collection
(use-package evil-collection
  :after evil
  :ensure t
  :init
  (evil-collection-init))
;;; evil-magit
(use-package evil-magit
  :after evil
  :ensure t
  :config
  (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward))
