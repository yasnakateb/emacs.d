;;; bindings.el --- Buffers -*- lexical-binding: t; -*-

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

;;; ibuffer 
(general-define-key
 :prefix "SPC b"
 :states '(normal visual motion)
 :keymaps 'override
 "d" 'kill-current-buffer
 "D" 'kill-buffer
 "b" 'helm-buffers-list
 "B" 'ibuffer)
;;; avy
(with-eval-after-load 'avy
  (general-define-key
   :prefix "SPC s"
   :states '(normal visual motion)
   :keymaps 'override
   "a" 'avy-goto-char))

;;; smart-hungry-delete
(with-eval-after-load 'smart-hungry-delete
  (general-define-key
   :prefix "<backspace>"
   :states '(insert)
   :keymaps 'override
   "" 'smart-hungry-delete-backward-char))

;;; expand-region
  (general-define-key
   :prefix "SPC s"
   :states '(normal visual motion)
   :keymaps 'override
   "e" 'er/expand-region)

;;; information
(general-define-key
   :prefix "SPC h"
   :states '(normal visual motion)
   :keymaps 'override
   "g" 'google-this
   "G" 'google-this-search
   "m" 'man)

;;; bindings.el ends here

