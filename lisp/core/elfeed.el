;;; elfeed
(use-package elfeed
  :ensure t
  :defer t)
;;; helper functions
(with-eval-after-load 'elfeed
  ;; add a star
  (defun elfeed-star ()
    "Apply starred to all selected entries."
    (interactive )
    (let* ((entries (elfeed-search-selected))
	   (tag (intern "starred")))
      (cl-loop for entry in entries do (elfeed-tag entry tag))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))
  ;; remove a start
  (defun elfeed-unstar ()
    "Remove starred tag from all selected entries."
    (interactive )
    (let* ((entries (elfeed-search-selected))
	   (tag (intern "starred")))
      (cl-loop for entry in entries do (elfeed-untag entry tag))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))
  ;; face for starred articles
  (defface elfeed-search-starred-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")
  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)
  ;;shortcut to jump to starred bookmark
  (defun elfeed-show-starred ()
    (interactive)
    (bookmark-jump "elfeed-starred"))
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'starred))
  ;;searches
  (defun elfeed-show-all ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-all"))
  (defun elfeed-show-emacs ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-emacs"))
  (defun elfeed-show-daily ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-daily"))
  (defun elfeed-show-network ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-network"))
  ;; makes sure elfeed reads index from disk before launching
  (defun elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))
  ;;write to disk when quiting
  (defun elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))
  )
