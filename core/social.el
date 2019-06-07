;;; md4rd mode for reddit
(use-package md4rd
  :ensure t
  :defer t)
;;; twittering mode
(use-package twittering-mode
  :ensure t
  :defer t
  :config
  (setq twittering-use-master-password t)
  (setq twittering-initial-timeline-spec-string
      '(":home"
        ":replies"
        ":search/emacs/"))
  (setq twittering-icon-mode t)
  (setq twittering-timer-interval 300)
  (setq twittering-url-show-status nil))
;;; ivy-youtube mode
(use-package ivy-youtube
  :ensure t
  :defer t
  :config
  (setq ivy-youtube-key 'youtube-key))
