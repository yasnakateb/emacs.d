;;; core-bindings.el --- Core key bindings -*- lexical-binding: t; -*-

;; Copyright (C) 2019  M.R. Siavash Katebzadeh

;; Author: M.R.Siavash Katebzadeh <mr.katebzadeh@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Esc
(global-set-key [escape] 'keyboard-escape-quit)

;;; leader file
(general-define-key
 :prefix "SPC"
 :states '(normal visual motion)
 :keymaps 'override
 "" '(nil :which-key "My lieutenant general prefix")
 "f" '(:ignore t :which-key "Files")
 "o" '(:ignore t :which-key "Org")
 "a" '(:ignore t :which-key "Applications")
 "g" '(:ignore t :which-key "Magit")
 "m" '(:ignore t :which-key "EMMS")
 "l" '(:ignore t :which-key "Local Bindings")
 "b" '(:ignore t :which-key "Buffers")
 "s" '(:ignore t :which-key "Search")
 "S" '(:ignore t :which-key "Shell")
 "h" '(:ignore t :which-key "Help!")
 "w" '(:ignore t :which-key "Windows")
 "t" '(:ignore t :which-key "Toggles")

 "x" 'helm-M-x
 ";" 'eval-expression
 )
(provide 'core-bindings)
;;; core-bindings.el ends here
