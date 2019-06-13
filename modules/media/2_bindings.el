;;; bindings.el --- Media -*- lexical-binding: t; -*-

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

;;; emms
(general-define-key
   :prefix "SPC m"
   :states '(normal visual motion)
   :keymaps 'override
   "a" 'emms-start-mpd
   "n" 'emms-player-mpd-next
   "p" 'emms-player-mpd-previous
   "s" 'emms-player-mpd-play
   "e" 'emms-player-mpd-pause)

;;; helm-youtube
(general-define-key
 :prefix "SPC h"
 :states '(normal visual motion)
 :keymaps 'override
 "Y" 'helm-youtube)

;;; transmission
(general-define-key
 :prefix "SPC a"
 :states '(normal visual motion)
 :keymaps 'override
 "T" 'transmission)

;;; bindings.el ends here
