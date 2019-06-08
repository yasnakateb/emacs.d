;;; leader file
(general-define-key
 :prefix "SPC"
 :keymaps 'normal
 "" '(nil :which-key "My lieutenant general prefix")
 ;; bind nothing but give SPC f a description for which-key
 "f" '(:ignore t :which-key "Files")
 "o" '(:ignore t :which-key "Org")
 "a" '(:ignore t :which-key "Applications")
 "g" '(:ignore t :which-key "Magit")
 "b" '(:ignore t :which-key "Buffers")
 "S" '(:ignore t :which-key "Search")
 "s" '(:ignore t :which-key "Shell")
 "h" '(:ignore t :which-key "Help!")
 "w" '(:ignore t :which-key "Windows")
 "t" '(:ignore t :which-key "Toggles")

 )
;;; Files
(general-define-key
   :prefix "SPC f"
   :states '(normal visual motion)
   :keymaps 'override
   "f" 'counsel-find-file
   "x" 'counsel-M-x
   )
;;; Applications
(general-define-key
   :prefix "SPC a"
   :states '(normal visual motion)
   :keymaps 'override
   "m" 'mu4e
   "t" 'twit
   "r" 'md4rd)
;;; Magit
(general-define-key
   :prefix "SPC g"
   :states '(normal visual motion)
   :keymaps 'override
   "s" 'magit-status)
;;; Org
(general-define-key
   :prefix "SPC o"
   :states '(normal visual motion)
   :keymaps 'override
   "o" 'org-mode)
;;; Buffers
(general-define-key
   :prefix "SPC b"
   :states '(normal visual motion)
   :keymaps 'override
   "b" 'ibuffer
   "d" 'kill-current-buffer
   "D" 'kill-buffer
   )
;;; Windows
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
   )
;;; Shell
(general-define-key
   :prefix "SPC s"
   :states '(normal visual motion)
   :keymaps 'override
   "s" 'eshell-toggle
   "u" 'update-ticket
   "c" 'scp-project
   "w" 'wttrin
   "W" 'wttrin-exit
   )
;;; Search 
(general-define-key
   :prefix "SPC S"
   :states '(normal visual motion)
   :keymaps 'override
   "c" 'avy-goto-char
   "s" 'swiper
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
   :prefix "SPC h"
   :states '(normal visual motion)
   :keymaps 'override
   "g" 'google-this
   "G" 'google-this-search
   "m" 'man
   )
;;; Toggles
(general-define-key
   :prefix "SPC t"
   :states '(normal visual motion)
   :keymaps 'override
   "n" 'neotree-toggle
   "I" 'rtags-imenu
   "r" 'darkroom-mode
   "t" 'toggle-transparency
   "i" 'imenu)
