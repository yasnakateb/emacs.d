(evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down
                                             (kbd "C-<SPC>") 'peep-dired-scroll-page-up
                                             (kbd "<backspace>") 'peep-dired-scroll-page-up
                                             (kbd "j") 'peep-dired-next-file
                                             (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
(evil-define-key 'normal dired-mode-map (kbd "/") 'dired-narrow)
(evil-define-key 'normal dired-mode-map (kbd "P") 'peep-dired)
(evil-define-key 'normal dired-mode-map (kbd "i") 'dired-subtree-insert)
(evil-define-key 'normal dired-mode-map (kbd "r") 'dired-subtree-remove)
(evil-define-key 'normal dired-mode-map (kbd "return") 'dired-single-buffer)
(evil-define-key 'normal dired-mode-map (kbd "l") 'dired-single-buffer)
(evil-define-key 'normal dired-mode-map (kbd "h") 'dired-single-up-directory)


















