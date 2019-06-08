;;; update ticket
(defun update-ticket()
  (interactive)
  (let ((command 
  (format
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
;;; Show weather
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-accept-language '("Accept-Language" . "en-US"))
  (setq wttrin-default-cities '("Edinburgh" "~Shiraz" "London")))