;;; packages.el --- Completion -*- lexical-binding: t; -*-

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

;;; company mode
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;;; auto-complete mode
(use-package auto-complete
  :ensure t
  :init
    (ac-config-default)
    (global-auto-complete-mode t))

;;; flycheck mode
(use-package flycheck
  :ensure t)

;;; yasnippet mode
(use-package yasnippet
  :ensure t)

;;; yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t)

;;; company-box
(use-package company-box
  :after company
  :delight
  :hook (company-mode . company-box-mode))

;;; abbrev
(use-package abbrev
  :ensure nil
  :delight
  :hook (text-mode . abbrev-mode)
  :custom (abbrev-file-name (concat mk-emacs-dir "abbrev_defs"))
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;;; packages.el ends here
