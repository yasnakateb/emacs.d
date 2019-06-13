;;; bindings.el --- Latex -*- lexical-binding: t; -*-

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

;;; Latex-local
(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'LaTeX-mode-map
 "c" 'TeX-command-master
 "\\" 'TeX-insert-macro
 "-" 'TeX-recenter-output-buffer
 "%" 'TeX-comment-or-uncomment-paragraph
 ";" 'TeX-comment-or-uncomment-region
 "a" 'TeX-command-run-all
 "k" 'TeX-kill-job
 "l" 'TeX-recenter-output-buffer
 "im" 'TeX-insert-macro
 "v" 'TeX-view
 "hd" 'TeX-doc
 "*" 'LaTeX-mark-section
 "." 'LaTeX-mark-environment
 "c" 'LaTeX-close-environment
 "e" 'LaTeX-environment
 "ii" 'LaTeX-insert-item
 "s" 'LaTeX-section
 "fe" 'LaTeX-fill-environment
 "fp" 'LaTeX-fill-paragraph
 "fr" 'LaTeX-fill-region
 "fs" 'LaTeX-fill-section
 "pb" 'preview-buffer
 "pc" 'preview-clearout
 "pd" 'preview-document
 "pe" 'preview-environment
 "pf" 'preview-cache-preamble
 "pp" 'preview-at-point
 "pr" 'preview-region
 "ps" 'preview-section
 "rc" 'reftex-citation
 "rg" 'reftex-grep-document
 "ri" 'reftex-index-selection-or-word
 "rI" 'reftex-display-index
 "r TAB" 'reftex-index
 "rl" 'reftex-label
 "rp" 'reftex-index-phrase-selection-or-word
 "rP" 'reftex-index-visit-phrases-buffer
 "rr" 'reftex-reference
 "rs" 'reftex-search-document
 "rt" 'reftex-toc
 "rT" 'reftex-toc-recenter
 "rv" 'reftex-view-crossref
 "rb" 'helm-bibtex)

;;; bindings.el ends here
