;;; bindings.el --- Elfeed -*- lexical-binding: t; -*-

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

;;; elfeed
(general-define-key
   :prefix "SPC a"
   :states '(normal visual motion)
   :keymaps 'override
   "e" 'elfeed)

(with-eval-after-load 'elfeed
  (evil-define-key 'normal elfeed-search-mode-map (kbd "A") 'elfeed-show-all
    (kbd "e") 'elfeed-show-emacs
    (kbd "d") 'elfeed-show-daily
    (kbd "a") 'elfeed-show-network
    (kbd "q") 'elfeed-save-db-and-bury
    (kbd "*") 'elfeed-star
    (kbd "8") 'elfeed-unstar
    (kbd "S") 'elfeed-show-starred
    (kbd "F") 'elfeed-toggle-star
    (kbd "U") 'elfeed-update))

;;; bindings.el ends here
