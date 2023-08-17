  (use-package org 

    :ensure
    :diminish 
    :config
    (progn
      (add-to-list 'org-modules 'org-habit)
      (add-to-list 'org-modules 'org-crypt)

      (setq org-M-RET-may-split-line nil)
      (setq org-agenda-files '("~/org/"))
      (setq org-agenda-ndays 1)
      (setq org-agenda-show-all-dates t)
      (setq org-agenda-show-inherited-tags 'always)
      (setq org-agenda-skip-deadline-if-done t)
      (setq org-agenda-skip-scheduled-if-done t)
      (setq org-agenda-start-on-weekday nil)
      (setq org-agenda-todo-ignore-with-date t) ;hide shed. and deadlined from global todo 
      (setq org-agenda-start-with-follow-mode nil)
      (setq org-agenda-text-search-extra-files '(agenda-archives))
      (setq org-catch-invisible-edits t)
      (setq org-deadline-warning-days 14)
      (setq org-default-notes-file "~/org/main.org")
      (setq org-edit-src-persistent-message nil)
      (setq org-enforce-todo-checkbox-dependencies t)
      (setq org-enforce-todo-dependencies t)
      (setq org-indent-mode t)
      (setq org-log-done 'time)
      (setq org-log-into-drawer t)
      (setq org-log-redeadline (quote time))
      (setq org-log-reschedule (quote time))
      (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
      (setq org-mobile-inbox-for-pull "~/org/inbox.org")
      ;;; interfered with dropbox sync
      ;;;(setq org-mobile-use-encryption t  )
      ;; org-mobile-encryption-password  set in secrets.el.gpg
      (setq org-outline-path-complete-in-steps nil)
      ;;(setq org-refile-allow-creating-parent-nodes (quote confirm)) ;
      ;;(setq org-refile-targets (quote ((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))))
      ;;(setq org-refile-use-outline-path 'buffer-name)
      (setq org-return-follows-link t)
      (setq org-reverse-note-order t)
      (setq org-show-hierarchy-above (quote ((default . t))))
      (setq org-show-siblings (quote ((default) (isearch) (bookmark-jump))))
      (setq org-use-tag-inheritance t)


  					  ;(setq org-hide-leading-stars t)


      (setq org-todo-keywords
  	    (quote ((sequence "TODO(t)" "NEXT(n)" "|"  "DONE(d!)")
  		    (sequence "WAITING(w@/!)" "|" "SOMEDAY(s)" "CANCELLED(c@/!)")
  		    )))

      (setq org-capture-templates
  	    (quote (("t" "todo" entry (file "~/org/inbox.org" )
  		     "* TODO  %? :FLAGGED:\n%U\n%a\n" )

  		    ("f" "fiction" entry (file+headline "~/org/serial-structure.org" "ideas")
  		     "* %? :NOTE:\n%U\n%a\n" )
  		    ("n" "note" entry (file+headline "~/org/notes.org" "notes")
  		     "* %? :NOTE:\n%U\n%a\n" )
  		    ;; ("i" "idea" entry (file+headline "~/org/notes.org" "ideas")
  		    ;;  "* %?\n" )
  		    ;; ("n" "information" entry (file+headline "~/org/index.org" "information")
  		    ;;  "* %?\n" )
  		    ("s" "shopping" checkitem
  		     (file+headline "~/org/main.org" "shopping ")
  		     "- [ ] %?\n")
  		    ("r" "rss" entry
  		     (file+olp "~/Dropbox/elfeed.org" "feeds" "captures")
  		     "*** %^C\n")
  		    ("j" "journal" entry (file+datetree "~/org/notes.org")
  		     "* %?\n %U\n  %i\n  %a")
  		    ("w" "work" entry (file+datetree "~/org/work.org")
  		     "* %?\n %U\n  %i\n  %a")
  		    )))  




      ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
      ;;

  					  ;(defvar my-org-mobile-sync-timer nil)

  					  ;(defvar my-org-mobile-sync-secs (* 60 20))

      ;; (defun my-org-mobile-sync-pull-and-push ()
      ;;   (org-mobile-pull)
      ;;   (org-mobile-push)
      ;;   (when (fboundp 'sauron-add-event)
      ;; 	(sauron-add-event 'my 3 "Called org-mobile-pull and org-mobile-push")))

      ;; (defun my-org-mobile-sync-start ()
      ;;   "Start automated `org-mobile-push'"
      ;;   (interactive)
      ;;   (setq my-org-mobile-sync-timer
      ;; 	    (run-with-idle-timer my-org-mobile-sync-secs t
      ;; 				 'my-org-mobile-sync-pull-and-push)))

      ;; (defun my-org-mobile-sync-stop ()
      ;;   "Stop automated `org-mobile-push'"
      ;;   (interactive)
      ;;   (cancel-timer my-org-mobile-sync-timer))

      ;; (my-org-mobile-sync-start)

      )
    :bind (
  	   ("C-c l" . org-store-link)
  	   ("C-c a" . org-agenda)
  	   ("C-c b" . org-switchb)
  	   ("C-c c" . org-capture)
  	   ))

  ;; (with-eval-after-load "org-mode"
  ;;      (evil-org-mode 1)
  ;;      )


  (with-eval-after-load "org-agenda" 
    (define-key org-agenda-mode-map (kbd "J") 'org-agenda-goto-date) 
    (define-key org-agenda-mode-map (kbd "j") 'evil-next-line) 
    (define-key org-agenda-mode-map (kbd "k") 'evil-previous-line) 

    )



  ;;put all DONE into archive
  (defun my-org-archive-done-tasks ()
    (interactive)
    (unless
  	(org-map-entries 'org-archive-subtree "/DONE" 'file)))


  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'after-save-hook 'my-org-archive-done-tasks 'make-it-local)))


