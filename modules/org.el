;;; org.el --- Org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-mode, calfw, org-babel

;;; Code:

(use-package org
  :ensure t
  :demand
  :config
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
  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-start-with-follow-mode nil)
  (setq org-agenda-text-search-extra-files '(agenda-archives))
  (setq org-catch-invisible-edits 'smart)
  (setq org-deadline-warning-days 14)
  (setq org-default-notes-file "~/org/main.org")
  (setq org-edit-src-persistent-message nil)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (setq org-mobile-inbox-for-pull "~/org/inbox.org")
  (setq org-outline-path-complete-in-steps nil)
  (setq org-return-follows-link t)
  (setq org-reverse-note-order t)
  (setq org-show-hierarchy-above '((default . t)))
  (setq org-show-siblings '((default) (isearch) (bookmark-jump)))
  (setq org-use-tag-inheritance t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "WAITING(w@/!)" "|" "SOMEDAY(s)" "CANCELLED(c@/!)")))

  (setq org-capture-templates
        '(("t" "todo" entry (file "~/org/inbox.org")
           "* TODO  %? :FLAGGED:\n%U\n%a\n")
          ("f" "fiction" entry (file+headline "~/org/serial-structure.org" "ideas")
           "* %? :NOTE:\n%U\n%a\n")
          ("n" "note" entry (file+headline "~/org/notes.org" "notes")
           "* %? :NOTE:\n%U\n%a\n")
          ("s" "shopping" checkitem (file+headline "~/org/main.org" "shopping ")
           "- [ ] %?\n")
          ("r" "rss" entry (file+olp "~/Dropbox/elfeed.org" "feeds" "captures")
           "*** %^C\n")
          ("j" "journal" entry (file+datetree "~/org/notes.org")
           "* %?\n %U\n  %i\n  %a")
          ("w" "work" entry (file+datetree "~/org/work.org")
           "* %?\n %U\n  %i\n  %a")))

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c c" . org-capture)))

;; Org-babel languages (restclient loaded separately in tools.el)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)
   (shell . t)
   (python . t)))

;; Calfw - calendar view
(use-package calfw
  :ensure t
  :config
  (require 'calfw)
  (use-package calfw-org
    :ensure t
    :config
    (require 'calfw-org)))

(provide 'org)
;;; org.el ends here
