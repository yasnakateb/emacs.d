;;; configs.el --- Tools -*- lexical-binding: t; -*-

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

;;; calc
(with-eval-after-load 'calc
  (setq math-additional-units '((GiB "1024 * MiB" "Giga Byte")
				(MiB "1024 * KiB" "Mega Byte")
				(KiB "1024 * B" "Kilo Byte")
				(B nil "Byte")
				(Gib "1024 * Mib" "Giga Bit")
				(Mib "1024 * Kib" "Mega Bit")
				(Kib "1024 * b" "Kilo Bit")
				(b "B / 8" "Bit"))
	math-units-table nil)) 

;;; calendar
(with-eval-after-load 'calendar
  (setq calendar-mark-holidays-flag t))

;;; holidays
(with-eval-after-load 'holidays
  (setq holiday-bahai-holidays nil
	holiday-christian-holidays '((holiday-fixed 1 6 "Epiphany")
				     (holiday-fixed 2 2 "Candlemas")
				     (holiday-easter-etc -47 "Mardi Gras")
				     (holiday-easter-etc 0 "Easter Day")
				     (holiday-easter-etc 1 "Easter Monday")
				     (holiday-easter-etc 39 "Ascension")
				     (holiday-easter-etc 49 "Pentecost")
				     (holiday-fixed 8 15 "Assumption")
				     (holiday-fixed 11 1 "All Saints' Day")
				     (holiday-fixed 11 2 "Day of the Dead")
				     (holiday-fixed 11 22 "Saint Cecilia's Day")
				     (holiday-fixed 12 1 "Saint Eloi's Day")
				     (holiday-fixed 12 4 "Saint Barbara")
				     (holiday-fixed 12 6 "Saint Nicholas Day")
				     (holiday-fixed 12 25 "Christmas Day"))
	holiday-general-holidays '((holiday-fixed 1 1 "New Year's Day")
				   (holiday-fixed 2 14 "Valentine's Day")
				   (holiday-fixed 3 8 "International Women's Day")
				   (holiday-fixed 10 31 "Halloween")
				   (holiday-fixed 11 11 "Armistice of 1918"))
	holiday-hebrew-holidays nil
	holiday-islamic-holidays nil
	holiday-local-holidays '((holiday-fixed 5 1 "Labor Day")
				 (holiday-float 3 0 0 "Grandmothers' Day")
				 (holiday-float 4 4 3 "Secretary's Day")
				 (holiday-float 5 0 2 "Mother's Day")
				 (holiday-float 6 0 3 "Father's Day"))
	holiday-oriental-holidays nil))

;;; configs.el ends here
