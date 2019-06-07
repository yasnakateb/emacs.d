;;; leader file
(general-define-key
   :prefix "SPC f"
   :states '(normal visual motion)
   :keymaps 'override
   "f" 'counsel-find-file
   "x" 'counsel-M-x
   "s" 'swiper
   "t" 'neotree-toggle
   "I" 'rtags-imenu
   "i" 'imenu
   )
;;; leader modes
(general-define-key
   :prefix "SPC m"
   :states '(normal visual motion)
   :keymaps 'override
   "m" 'mu4e
   "g" 'magit-status
   "o" 'org-mode
   "t" 'twit
   "r" 'md4rd
   )
;;; leader buffer
(general-define-key
   :prefix "SPC b"
   :states '(normal visual motion)
   :keymaps 'override
   "b" 'ibuffer
   "d" 'kill-current-buffer
   "D" 'kill-buffer
   "r" 'darkroom-mode
   )
;;; leader window
(general-define-key
   :prefix "SPC w"
   :states '(normal visual motion)
   :keymaps 'override
   "v" 'evil-window-vsplit
   "s" 'evil-window-split
   "d" 'evil-window-delete
   "h" 'evil-window-left
   "j" 'evil-window-down
   "k" 'evil-window-up
   "l" 'evil-window-right
   "t" 'toggle-transparency
   )
;;; leader shell
(general-define-key
   :prefix "SPC s"
   :states '(normal visual motion)
   :keymaps 'override
   "u" 'update-ticket
   "s" 'scp-project
   "w" 'wttrin
   "W" 'wttrin-exit
   )
;;; leader goto 
(general-define-key
   :prefix "SPC g"
   :states '(normal visual motion)
   :keymaps 'override
   "c" 'avy-goto-char
   )
;;; leader config files 
(defun configs-visit ()
(interactive)
(find-file "~/.emacs.d/configs"))
(general-define-key
   :prefix "SPC C"
   :states '(normal visual motion)
   :keymaps 'override
   "C" 'configs-visit 
   )
;;; information
(general-define-key
   :prefix "SPC i"
   :states '(normal visual motion)
   :keymaps 'override
   "g" 'google-this
   "G" 'google-this-search
   "m" 'man
   )
