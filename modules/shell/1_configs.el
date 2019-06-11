;;; configs.el --- Shell -*- lexical-binding: t; -*-

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

(require 'em-tramp)
(setq password-cache t
      password-cache-expiry 3600)

(with-eval-after-load 'esh-module
  ;; REVIEW: It used to work, but now the early `provide' seems to backfire.
  (unless (boundp 'eshell-modules-list)
    (load "esh-module"))
  ;; Don't print the banner.
  (delq 'eshell-banner eshell-modules-list)
  (push 'eshell-tramp eshell-modules-list))

(setq eshell-ls-use-colors t
      ;; ffap-shell-prompt-regexp changes the behaviour of `helm-find-files' when
      ;; point is on prompt. I find this disturbing.
      ffap-shell-prompt-regexp nil
      eshell-history-size 1024
      eshell-hist-ignoredups t
      eshell-destroy-buffer-when-process-dies t)

(defun eshell-prompt ()
  (let ((path (abbreviate-file-name (eshell/pwd))))
    (concat
     (when eshell-status-p
       (propertize (or (eshell-status-display) "") 'face font-lock-comment-face))
     (format
      (propertize "(%s@%s)" 'face '(:weight bold))
      (propertize (user-login-name) 'face '(:foreground "cyan"))
      (propertize (system-name) 'face '(:foreground "cyan")))
     (if (and (require 'magit nil t) (or (magit-get-current-branch) (magit-get-current-tag)))
         ;; We need "--show-prefix and not "--top-level" when we don't follow symlinks.
         (let* ((git-file-path (abbreviate-file-name (magit-rev-parse "--show-prefix")))
                prefix)
           (let ((common-folder (car (split-string git-file-path "/"))))
             (setq prefix (mapconcat 'identity (seq-take-while
                                                (lambda (s) (not (string= s common-folder)))
                                                (split-string path "/"))
                                     "/")))
           (format
            (propertize "[%s/%s@%s]" 'face '(:weight bold))
            (propertize prefix 'face `(:foreground ,(if (= (user-uid) 0) "red" "green") :weight bold))
            (propertize (substring-no-properties path (min (length path) (1+ (length prefix))))
                        'face `(:foreground ,(if (= (user-uid) 0) "orange" "gold")))
            (or (magit-get-current-branch) (magit-get-current-tag))))
       (format
        (propertize "[%s]" 'face '(:weight bold))
        (propertize path 'face `(:foreground ,(if (= (user-uid) 0) "red" "green") :weight bold))))
     (propertize "\n>" 'face '(:weight bold))
     " ")))

;;; Leave `eshell-highlight-prompt' to t as it sets the read-only property.
(setq eshell-prompt-function #'eshell-prompt)
;;; If the prompt spans over multiple lines, the regexp should match
;;; last line only.
(setq-default eshell-prompt-regexp "^> ")

(with-eval-after-load 'em-term
  (dolist (p '("abook" "alsamixer" "fzf" "htop" "mpsyt" "mpv" "mutt" "newsbeuter" "watch" "wifi-menu"))
    (add-to-list 'eshell-visual-commands p))
  (setq eshell-visual-subcommands
        ;; Some Git commands use a pager by default.
        ;; Either invoke the subcommands in a term ("visual") or configure Git
        ;; to disable the pager globally.
        ;; '(("git" "log" "diff" "show")
        '(("sudo" "wifi-menu") ; Arch Linux
          ("sudo" "vi" "visudo"))))

(with-eval-after-load 'em-alias
  (dolist
      (alias
       '(("l" "ls -1 $*")
         ("la" "ls -lAh $*")
         ("ll" "ls -lh $*")
         ("cpv" "cp -iv $*")
         ("mvv" "mv -iv $*")
         ("rmv" "rm -v $*")
         ("md" "eshell/mkdir -p $*")
         ("mkcd" "eshell/mkdir -p $* ; cd $1"))) ; TODO: '&&' does not work because mkdir exits with nil?
    (add-to-list 'eshell-command-aliases-list alias))
  (eshell-write-aliases-list))

;;; Hooks
;;; `nobreak-char-display' makes some output look weird, e.g. with 'tree'.

(setq eshell-input-filter
      (lambda (str)
        (not (or
              ;; Here we can filter out failing commands.  This is usually a bad
              ;; idea since a lot of useful commands have non-zero exit codes
              ;; (including Emacs/Eshell functions).
              ;; (/= eshell-last-command-status 0)
              (string= "" str)
              (string= "cd" str)
              (string-prefix-p "cd " str)
              ;; Filter out space-beginning commands from history.
              (string-prefix-p " " str)))))

;;; Shared history.
(defvar eshell-history-global-ring nil
  "The history ring shared across Eshell sessions.")

(defun eshell-hist-use-global-history ()
  "Make Eshell history shared across different sessions."
  (unless eshell-history-global-ring
    (when eshell-history-file-name
      (eshell-read-history nil t))
    (setq eshell-history-global-ring (or eshell-history-ring (make-ring eshell-history-size))))
  (setq eshell-history-ring eshell-history-global-ring))
(add-hook 'eshell-mode-hook 'eshell-hist-use-global-history)

(defun eshell-history-remove-duplicates ()
  "Remove duplicates of last command in history.
This should be faster then `seq-uniq'."
  (let ((first (ring-ref eshell-history-ring 0))
        (index 1))
    (while (<= index (1- (ring-length eshell-history-ring)))
      (if (string= first (ring-ref eshell-history-ring index))
          ;; REVIEW: We could stop at the first match, it would be faster and it
          ;; would eliminate duplicates if we started from a fresh history.
          ;; From an existing history that would not clean up existing
          ;; duplicates beyond the first one.
          (ring-remove eshell-history-ring index)
        (setq index (1+ index))))))
(add-hook 'eshell-pre-command-hook 'eshell-history-remove-duplicates)

;; Always save history
(add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

;;; Spawning
(when (require 'patch-helm nil 'noerror)
  (helm-defswitcher
   "Eshell"
   (lambda (b)
     (with-current-buffer b
       (or
        (derived-mode-p 'eshell-mode)
        (derived-mode-p 'shell-mode))))
   (lambda (&optional name)
     ;; TODO: Could add an option "prefer-eshell" to choose between Eshell/Shell by default.
     (eshell 'new)
     (when name
       (rename-buffer (format "*eshell<%s>*" name) 'unique)))
   nil
   (helm-build-dummy-source
       "Create shell buffer"
     :action `(("Create shell buffer" .
                (lambda (candidate)
                  (let ((new-buffer (save-window-excursion (shell (format "*shell<%s>*" candidate))
                                                           (current-buffer))))
                    (switch-to-buffer new-buffer))))))))

;;; Auto-suggestion
(when (require 'esh-autosuggest nil t)
  (setq esh-autosuggest-delay 0.75)
  (add-hook 'eshell-mode-hook 'esh-autosuggest-mode)
  (define-key esh-autosuggest-active-map (kbd "<tab>") 'company-complete-selection)
  (when (require 'helm-config nil t)
    (define-key company-active-map (kbd "M-p") 'helm-eshell-history)))

;;; Extra execution information
(defvar eshell-status-p t
  "If non-nil, display status before prompt.")
(defvar eshell-status--last-command-time nil)
(make-variable-buffer-local 'eshell-status--last-command-time)
(defvar eshell-status-min-duration-before-display 1
  "If a command takes more time than this, display its duration.")

(defun eshell-status-display ()
  (when eshell-status--last-command-time
    (let ((duration (time-subtract (current-time) eshell-status--last-command-time)))
      (setq eshell-status--last-command-time nil)
      (when (> (time-to-seconds duration) eshell-status-min-duration-before-display)
        (format "#[STATUS] End time %s, duration %.3fs\n"
                (format-time-string "%F %T" (current-time))
                (time-to-seconds duration))))))

(defun eshell-status-record ()
  (setq eshell-status--last-command-time (current-time)))

(add-hook 'eshell-pre-command-hook 'eshell-status-record)

;;; Detach
(when (require 'package-eshell-detach nil t)
  (defun eshell-detach-set-keys ()
    (define-key eshell-mode-map (kbd "C-c C-z") 'eshell-detach-stop)
    (define-key eshell-mode-map (kbd "S-<return>") 'eshell-detach-send-input)
    (define-key eshell-mode-map (kbd "C-<return>") 'eshell-detach-attach))
  (add-hook 'eshell-mode-hook 'eshell-detach-set-keys))

;; Man
(when (string= (file-symlink-p (executable-find "man")) "mandoc")
  ;; Some systems like Void Linux use mandoc instead of man and do not know the
  ;; --nj, --nh flags.
  (defun pcmpl-args-mandoc-man-function (name)
    (let ((process-environment process-environment))
      ;; Setting MANWIDTH to a high number makes most paragraphs fit on a single
      ;; line, reducing the number of false positives that result from lines
      ;; starting with `-' that aren't really options.
      (push "MANWIDTH=10000" process-environment)
      (pcmpl-args-process-file "man" "--" name)))
  (setq pcmpl-args-man-function 'pcmpl-args-mandoc-man-function))

;; Completion
(when (require 'bash-completion nil t)
  (when (and (or (executable-find "fish")
                 ;; "fish" needs not be in PATH with guix.
                 (executable-find "guix"))
             (require 'fish-completion nil t))
    (setq fish-completion-fallback-on-bash-p t)
    (global-fish-completion-mode)))

;; Misc.
(defun eshell-append-region-to-command-line (begin end)
  (interactive "r")
  (require 'subr-x)                     ; For `string-trim'.
  (save-mark-and-excursion
    (let ((string (buffer-substring-no-properties begin end)))
      (setq string (string-trim string))
      (setq string (concat string " "))
      (goto-char (point-max))
      (ignore-errors (cycle-spacing 0))
      (insert string))))


;;; update ticket
(defun update-ticket()
  (interactive)
  (let ((command (format
		  "echo %s | kinit %s@%s && cd %s && pssh -h %s -l %s 'echo %s | kinit; aklog -force'"
		  staff-password
		  "s1691546"
		  staff-realm
		  staff-phd
		  staff-hosts
		  "s1691546"
		  staff-password)))
    (shell-command command)))

;;; scp-project
(defun scp-project ()
  (interactive)
  (let*
     ((dir 
         (file-name-nondirectory
            (directory-file-name
               (file-name-directory
                 (projectile-project-root)))))

        (command
          (format
            "rsync -rav -e ssh   --exclude=build --exclude='.git*' %s %s%s"
	       (projectile-project-root)
	       staff-project
	       dir)))
  (shell-command command)))

(defun shell-exit () 
  (when (not (one-window-p))
    (delete-window)))

(advice-add 'eshell-life-is-too-much :after 'shell-exit)
(require 'eshell-toggle)

;;; configs.el ends here
