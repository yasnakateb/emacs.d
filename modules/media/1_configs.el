;;; configs.el --- Media -*- lexical-binding: t; -*-

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
(require 'emms-setup)
(require 'emms-player-mpd)
(emms-all)
(setq emms-seek-seconds 5)
(setq emms-player-list '(emms-player-mpd))
(setq emms-info-functions '(emms-info-mpd))
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")

(setq mpc-host "localhost:6600")
(defun mpd-start-music-daemon ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (shell-command "mpd")
  (mpd-update-database)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "MPD Started!"))

(defun mpd-kill-music-daemon ()
  "Stops playback and kill the music daemon."
  (interactive)
  (emms-stop)
  (call-process "killall" nil nil nil "mpd")
  (message "MPD Killed!"))

(defun mpd-update-database ()
  "Updates the MPD database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD Database Updated!"))

(defun emms-start-mpd ()
  "Run EMMS and MPD"
  (interactive)
  (mpd-start-music-daemon)
  (emms))

;;; helm-youtube
(with-eval-after-load 'helm-youtube
  (load-library (concat mk-secret-dir "youtube.gpg")))

;;; configs.el ends here

