(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(require 'use-package)
(setq straight-use-package-by-default t)

(use-package diminish :ensure t)
(diminish 'subword-mode)
(diminish 'visual-line-mode)
(diminish 'auto-revert-mode)

(use-package no-littering :ensure t)

(use-package bind-key
  ;; A simple way to manage personal keybindings, provided by `use-package'
  :demand
  :init
  ;; If non-nil, extract docstrings from lambdas, closures and keymaps if possible.
  (setq bind-key-describe-special-forms t)
  :config
  (require 'bind-key)
  )

(use-package org
  :ensure t
  :demand
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
    (setq org-catch-invisible-edits 'smart)
    (setq org-deadline-warning-days 14)
    (setq org-default-notes-file "~/org/main.org")
    (setq org-edit-src-persistent-message nil)
    (setq org-enforce-todo-checkbox-dependencies t)
    (setq org-enforce-todo-dependencies t)
    (add-hook 'org-mode-hook 'org-indent-mode)
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


;; (with-eval-after-load "org-agenda"
;;   (define-key org-agenda-mode-map (kbd "J") 'org-agenda-goto-date)
;;   (define-key org-agenda-mode-map (kbd "j") 'evil-next-line)
;;   (define-key org-agenda-mode-map (kbd "k") 'evil-previous-line)

;;   )



;;put all DONE into archive
;; (defun my-org-archive-done-tasks ()
;;   (interactive)
;;   (unless
;;       (org-map-entries 'org-archive-subtree "/DONE" 'file)))


;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook 'my-org-archive-done-tasks 'make-it-local)))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)
   (shell . t)
   (python . t)
   (restclient . t)))

(defun org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/init.org"))
    (let ((org-config-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook #'org-babel-tangle-config)))

(use-package evil
  :ensure t
  :init
  ;;turn on for evil collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
                                        ;(evil-mode 1)
                                        ;(evil-set-undo-system 'undo-redo)

  )

(use-package evil-collection
  :after evil
  :diminish
  :ensure t
  :config
  (setq evil-collection-want-unimpaired-p nil)
  ;; (evil-collection-init)
  )



(use-package evil-visual-mark-mode
  :ensure t
  :config
  (evil-visual-mark-mode 1))

(use-package evil-org
  :ensure t
  :after org
  :diminish
  ;;:hook (org-mode . (lambda () (evil-org-mode +1)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package dired
  :straight nil				;
  :bind (:map dired-mode-map
	      (("`" . dired-toggle-read-only)
	       ( "-" .  dired-up-directory)
	       ("~" . (lambda ()(interactive) (find-alternate-file "~/")))
	       ("RET" . dired-find-file)
	       ("C-<return>" . dired-find-file-other-window)))
  :config
  (use-package dired+
    :straight (dired+ :fetcher url :url "https://www.emacswiki.org/emacs/download/dired+.el")
    :defer 1
    :init
    (setq diredp-hide-details-initially-flag t)
    (setq diredp-hide-details-propagate-flag t)
    :config
    (diredp-toggle-find-file-reuse-dir 1))

  )


(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

(use-package dired-hist
  :straight (:host github :repo "karthink/dired-hist" :files ( "*.el"))
  :ensure t
  :bind (:map dired-mode-map
	      ("l" . dired-hist-go-back)
	      ("r" . dired-hist-go-forward))
  :config
  (dired-hist-mode 1)
  )

(use-package ag :ensure t :defer t :config (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't))

(use-package rg :ensure t :defer t
  :config
  (rg-enable-default-bindings)
  )

(use-package calfw :ensure t
  :config
  (require 'calfw)
  (use-package calfw-org :ensure t
    :config
    (require 'calfw-org)
    ;;   (setq cfw:org-overwrite-default-keybinding t)
    )
  )

(use-package company
  :ensure t
  :diminish )

(use-package gptel
  :ensure t
  :config
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (getenv "ANTHROPIC_API_KEY"))
        gptel-model 'claude-opus-4-20250514
        gptel-default-mode 'org-mode))

(use-package project-x
  :straight (:host github :repo "karthink/project-x" :files ( "*.el"))
  :after project
  :config
  (setq project-x-save-interval 600)    ;Save project state every 10 min
  (project-x-mode 1))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  ;; :hook (prog-mode . copilot-mode)	;
  :config
  (progn
    (setq copilot-expansion-delay 0.2)
    (setq copilot-expansion-limit 500)
    (setq copilot-node-executable (executable-find "node"))
    )

  :bind (("C-c M-f" . copilot-complete)
	 :map copilot-completion-map
	 ("C-g" . 'copilot-clear-overlay)
	 ("M-p" . 'copilot-previous-completion)
	 ("M-n" . 'copilot-next-completion)
	 ("<tab>" . 'copilot-accept-completion)
	 ("M-f" . 'copilot-accept-completion-by-word)
	 ("M-<return>" . 'copilot-accept-completion-by-line)))

(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package flycheck
  :ensure t
  :diminish ""
  :config
  ;; (progn
  ;;   (add-hook 'after-init-hook 'global-flycheck-mode))
  )


(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
	    (id (one-or-more (not (any " "))))
	    (message) line-end))
  :modes (text-mode prose-mode markdown-mode gfm-mode))

(add-to-list 'flycheck-checkers 'proselint)

;; (use-package apheleia
;;   :ensure t
;;   :diminish
;;   :config
;;   (apheleia-global-mode t))
;; (use-package prettier
;;   :ensure t
;;   :diminish
;;   :config
;;   (global-prettier-mode))

(use-package dtrt-indent
  :diminish
  :ensure t
  :init
  (dtrt-indent-global-mode 1))

(use-package general
  :ensure t
  :defer 1
  :config   (setq leader "SPC"))

(use-package git-link
  :ensure t
  :diminish
  )

(use-package git-gutter
  :ensure t
  :diminish
  :config
  (global-git-gutter-mode t)
  
  ;; If you would like to use git-gutter.el and linum-mode
  ;; (git-gutter:linum-setup)

  ;; If you enable git-gutter-mode for some modes

  (global-set-key (kbd "C-x C-g") 'git-gutter)
  (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

  ;; Jump to next/previous hunk
  (global-set-key (kbd "C-x C-p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x C-n") 'git-gutter:next-hunk)

  (setq git-gutter:update-interval 0.2)
  ;; Stage current hunk
  (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

  ;; Revert current hunk
  (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

  ;; Mark current hunk
  (global-set-key (kbd "C-x v SPC") #'git-gutter:mark-hunk)
  )

(use-package image+
  :ensure hydra
  :defer t
  :config
  (progn
    (eval-after-load 'image+
      `(when (require 'hydra nil t)
	 (defhydra imagex-sticky-binding (global-map "C-x C-l")
		   "Manipulating Image"
		   ("+" imagex-sticky-zoom-in "zoom in")
		   ("-" imagex-sticky-zoom-out "zoom out")
		   ("M" imagex-sticky-maximize "maximize")
		   ("O" imagex-sticky-restore-original "restore original")
		   ("S" imagex-sticky-save-image "save file")
		   ("r" imagex-sticky-rotate-right "rotate right")
		   ("l" imagex-sticky-rotate-left "rotate left"))))
    )
  )

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
	'(pixel-scroll-precision
	  mwheel-scroll
	  self-insert-command)))

(use-package magit
  :ensure t
  :defer t
  :config
  (define-key transient-map (kbd "<escape>") 'transient-quit-one)
  (setq magit-save-repository-buffers 'dontask)
  )
(use-package forge
  :after magit
  :init
  (setq forge-add-default-bindings t)
  :config
  (setq auth-sources '("~/.authinfo")))

(use-package better-defaults :ensure t)
(use-package bind-map :ensure t :defer t)
(use-package emojify :ensure t :defer t )
(use-package markdown-mode :ensure t :defer t)
(use-package smex :ensure t :defer t)
(use-package feebleline :ensure t :defer t)
(use-package fzf :ensure t :defer t)
(use-package origami :ensure t :defer t )
(use-package swiper :ensure t )
;;(use-package recursive-narrow :ensure t :defer t)

(use-package page-break-lines
  :diminish ""
  :ensure t
  :defer t
  :config (global-page-break-lines-mode))

(use-package persistent-scratch :ensure t
  :config (persistent-scratch-setup-default))

(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode +1))

(use-package savehist
  :config
  (setq savehist-file "~/.emacs.d/var/savehist")
  (setq
   savehist-additional-variables
   '(kill-ring
     mark-ring
     global-mark-ring
     search-ring
     regexp-search-ring
     extended-command-history))
  (savehist-mode 1))

(use-package saveplace :config (setq-default save-place t))

(use-package term )

(use-package writeroom-mode
  :ensure t
  :defer t)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;uncomment and update these every so often
;;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))


(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)))

(use-package which-key
  :ensure t
  :diminish ""
  :config (which-key-mode ) )

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;;      (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;;     (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; (use-package hotfuzz
;;   :init
;;   (setq completion-styles '(hotfuzz basic)
;;   	completion-ignore-case t
;; 	read-buffer-completion-ignore-case t
;; 	read-file-name-completion-ignore-case t

;;   	completion-category-defaults nil
;;   	completion-category-overrides '((file (styles partial-completion)))
;;   	))
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(use-package expand-region :ensure t)

(use-package avy
  :ensure t
  :demand t
  :bind (("M-j"   . avy-goto-char-timer)))

;; Popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :straight nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package speed-type :ensure t
  :custom
  (speed-type-default-lang 'English))

(use-package visible-mark :ensure t :config (global-visible-mark-mode 1))

(use-package golden-ratio
  :ensure t
  :diminish ""
  :config
  (golden-ratio-mode 1)

  (setq golden-ratio-exclude-modes '(ediff-mode))
  ;; (setq golden-ratio-exclude-buffer-names '("..."))
  )

(use-package hcl-mode
:ensure t
:mode (("\\.tf\\'" . hcl-mode)
       ("\\.tfvars\\'" . hcl-mode))
:config
;; Additional configuration specific to hcl-mode
)

(use-package highlight-indent-guides
  :ensure t
  :diminish
  :hook (yaml-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode);
  :config
  (use-package ob-restclient
    :ensure t
    :config
    (setq org-babel-restclient--jq-path "/opt/homebrew/bin/jq")))

(use-package eldoc
  :diminish
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode))


   ;;;; elisp-slime-nav
;; jump to elisp definition (function, symbol etc.) and back, show doc
;; (use-package elisp-slime-nav
;;   :demand
;;   :quelpa (elisp-slime-nav :repo "purcell/elisp-slime-nav" :fetcher github)
;;   :bind
;;   ("<f1> <f1>" . elisp-slime-nav-describe-elisp-thing-at-point)
;;   :diminish
;;   :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . elisp-slime-nav-mode))

(use-package clojure-mode :ensure t :defer t)
(use-package cider :ensure t :defer t
  )
					;  (use-package inf-clojure :ensure t)
(add-hook 'clojure-mode-hook #'eldoc-mode)

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

(use-package jedi :ensure t :defer t
  :config
  (progn
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)))
(use-package ob-ipython :ensure t :defer t)
                                        ;(use-package ein :ensure)

(use-package jest-test-mode
  :ensure t
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))

(use-package indium
  :ensure t
  :hook ((js-mode . indium-interaction-mode)
         (js2-mode . indium-interaction-mode)
         (typescript-mode . indium-interaction-mode))
  :config
  ;; If you have any custom configuration, place it here.
  )

(use-package smartscan
  :ensure t
  :demand t
  :hook
  ((prog-mode-hook . smartscan-mode )))

(use-package combobulate
  :straight (:host github :repo "mickeynp/combobulate")
  :preface
  (setq combobulate-key-prefix "C-c o")
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode))
  :bind (:map paredit-mode-map
              ("C-j" . paredit-newline)))

;;taken from https://macowners.club/posts/email-emacs-mu4e-macos/#storing-trusted-root-certificates
;;and https://rakhim.org/fastmail-setup-with-emacs-mu4e-and-mbsync-on-macos/


(use-package mu4e
  :straight nil
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/"
  :config
  (require 'mu4e-contrib)
  (require 'smtpmail)
  (setq
   message-send-mail-function 'sendmail-send-it
   message-sendmail-envelope-from 'header
   send-mail-function 'sendmail-send-it
   sendmail-program (executable-find "msmtp")

   mu4e-attachments-dir "~/Downloads"
   mu4e-change-filenames-when-moving t
   mu4e-completing-read-function 'completing-read
   mu4e-compose-format-flowed nil
   mu4e-date-format "%y/%m/%d"
   mu4e-get-mail-command  "mbsync -a"
   mu4e-headers-date-format "%Y/%m/%d"
   mu4e-mu-binary "/opt/homebrew/bin/mu"
   mu4e-read-option-use-builtin nil
   mu4e-view-show-addresses t
   mu4e-view-show-images t
   mu4e-headers-skip-duplicates t)

  (setq mu4e-contexts
        `(


          ,(make-mu4e-context
            :name "Fastmail"
            :enter-func (lambda () (mu4e-message "Switch to the Fastmail context"))
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg :to "logan@mohseni.io")))
            :vars '(
		    (user-full-name         . "Logan Mohseni" )
                    (mu4e-compose-signature  . (concat "\n\n--Logan Mohseni\n"))
            	    (mu4e-maildir            . "~/Maildir/fastmail" )
            	    (mu4e-refile-folder      . "/fastmail/Archive")
            	    (mu4e-sent-folder        . "/fastmail/Sent")
            	    (mu4e-drafts-folder      . "/fastmail/Drafts")
            	    (mu4e-trash-folder       . "/fastmail/Trash")
            	    (mu4e-maildir-shortcuts  . (
            					("/fastmail/Banking" . ?b)
            					("/fastmail/Bills" . ?B)
            					("/fastmail/Reading" . ?r)
            					("/fastmail/Lists/OpenBSD" . ?p)
            					("/fastmail/Lists/Org" . ?O)
            					("/fastmail/Lists/Sbcl" . ?s)
            					("/fastmail/Lists/Emacs" . ?e)
            					("/fastmail/Shopping/Amazon" . ?A)
            					("/fastmail/House Hunt" . ?h)
            					("/fastmail/Shipping" . ?R)))

            	    (mu4e-bookmarks          . (
            					(:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
            					(:name "Today's messages" :query "date:today..now" :key ?g)
            					(:name "Last 7 days" :query "date:7d..now" :hide-unread t :key ?w)
            					(:name "Messages with images" :query "mime:image/*" :key ?p)
            					(:query "maildir:/fastmail/Inbox" :name   "Inbox" :key   ?i)
            					(:query "maildir:/fastmail/Sent" :name "Sent Mail" :key   ?s)
            					(:query "maildir:/fastmail/Drafts" :name  "Drafts" :key ?d)
            					(:query "maildir:/fastmail/Archive" :name    "Archive" :key    ?x)))
		    ))

	  ,(make-mu4e-context
            :name "Gmail"
            :enter-func (lambda () (mu4e-message "Entering Private context"))
            :leave-func (lambda () (mu4e-message "Leaving Private context"))
            ;; we match based on the contact-fields of the message
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg :to "mohsenil85@gmail.com")))
            :vars '( (user-mail-address      . "mohsenil85@gmail.com"  )
                     (user-full-name         . "Logan Mohseni" )
                     (mu4e-compose-signature . (concat "\n\n--Logan Mohseni\n"))
            	     (mu4e-maildir           . "~/Maildir/gmail" )
            	     (mu4e-refile-folder     . "/gmail/Archive")
            	     (mu4e-sent-folder       . "/gmail/Sent")
            	     (mu4e-drafts-folder     . "/gmail/Drafts")

            	     (mu4e-trash-folder      . "/gmail/Trash")
            	     (mu4e-maildir-shortcuts  . (
            					 ("/gmail/Banking" . ?b)
            					 ("/gmail/Bills" . ?B)
            					 ("/gmail/Reading" . ?r)
            					 ("/gmail/Lists/OpenBSD" . ?p)
            					 ("/gmail/Lists/Org" . ?O)
            					 ("/gmail/Lists/Sbcl" . ?s)
            					 ("/gmail/Lists/Emacs" . ?e)
            					 ("/gmail/Shopping/Amazon" . ?A)
            					 ("/gmail/House Hunt" . ?h)
            					 ("/gmail/Shipping" . ?S)))

            	     (mu4e-bookmarks          . (
            					 (:name "Unread messages" :query "flag:unread AND NOT flag:trashed AND to:mohsenil85@gmail.com" :key ?u)
            					 (:name "Today's messages" :query "date:today..now" :key ?g)
            					 (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key ?w)
            					 (:name "Messages with images" :query "mime:image/*" :key ?p)
            					 (:query "maildir:/gmail/Inbox" :name   "Inbox" :key   ?i)
            					 (:query "maildir:/gmail/Sent" :name "Sent Mail" :key   ?s)
            					 (:query "maildir:/gmail/Drafts" :name  "Drafts" :key ?d)
            					 (:query "maildir:/gmail/Archive" :name    "Archive" :key    ?x)))
		     ))

  	  );;list
        );;contexts


  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.

  ;; start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask
  ;; (setq mu4e-compose-context-policy nil)

  )

(use-package emacs
    :init
    (require 'misc)
    ;; Add prompt indicator to `completing-read-multiple'.
    ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string
                     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                     crm-separator)
                    (car args))
            (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
    ;; Vertico commands are hidden in normal buffers.
    ;; (setq read-extended-command-predicate
    ;;       #'command-completion-default-include-p)


    (blink-cursor-mode -1)
    (defalias 'yes-or-no-p 'y-or-n-p)
    (delete-selection-mode 1)
    (electric-pair-mode 1)
    (size-indication-mode 1)
    (global-display-line-numbers-mode 1)
      ;;;        (visual-fill-column-mode 1)
    (toggle-word-wrap 1)
    ;;      (global-visual-line-mode t)
    (menu-bar-mode 0)
    (prefer-coding-system 'utf-8)
    (recentf-mode 1)
    (scroll-bar-mode 0)
;;    (server-start)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8-unix)
    (tool-bar-mode 0)
    (tooltip-mode -1)
    (setq tooltip-use-echo-area t)
    (context-menu-mode)
    (pixel-scroll-precision-mode)
    (column-number-mode 1)

    (setq
     view-read-only t
     xref-search-program 'ripgrep
     ;;     sentence-end-double-space nil
     display-time-default-load-average nil
     auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
     auto-save-visited-interval 1
     auto-save-visited-mode 1
     backup-by-copying t
     backup-directory-alist `((".*" . ,temporary-file-directory))
     confirm-kill-processes nil
     confirm-nonexistent-file-or-buffer nil
     default-fill-column 80		; toggle wrapping text at the 80th character
     delete-old-versions t 		; delete excess backup versions silently
     ;; enable-recursive-minibuffers t
     explicit-shell-file-name "/bin/zsh"
     explicit-zsh-args '("--login" "--interactive")
     history-length 250
     indicate-empty-lines t
     inhibit-startup-echo-area-message "loganmohseni"
     inhibit-startup-message t
     inhibit-startup-screen t
     initial-scratch-message ";         :D"
     kill-ring-max 5000                     ;truncate kill ring after 5000 entries
     load-prefer-newer t
     locale-coding-system 'utf-8
     mark-ring-max 5000
     recentf-max-saved-items 5000
     ring-bell-function 'ignore 	; silent bell when you make a mistake
     sentence-end-double-space t	;
     shell-file-name "/bin/zsh"
     show-paren-delay 0
     show-paren-style 'parenthesis
     show-paren-when-point-inside-paren t
     ;;     split-width-threshold 80
     switch-to-buffer-preserve-window-point t
     tab-always-indent 'complete
     tooltip-use-echo-area t
     use-dialog-box nil
     user-full-name "Logan Mohseni"
     user-mail-address "logan@mohseni.io"
     vc-follow-symlinks t 				       ; don't ask for confirmation when opening symlinked file
     vc-make-backup-files t 		; make backups file even when in version controlled dir
     version-control t 		; use version control
     visible-bell t
     )
    (setq-default indicate-buffer-boundaries 'left)
    (setq display-time-format "%l:%M")
    (setq display-time-interval 1)
    (display-time-mode)

    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

    )




  (defun zsh-shell-mode-setup ()
    (setq-local comint-process-echoes t))
  (add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
  (winner-mode 1)


  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;(require 'cl)

(defun copy-filename-to-clip ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (with-temp-buffer
	(insert filename)
	(clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun load-init-file ()
  (interactive)
  (message "loading init...")
  (load-file (concat "~/.emacs.d/init.el")))

(defun edit-init-org-file ()
  (interactive)
  (if (string= buffer-file-name  (expand-file-name ".emacs.d/init.org" "~") ) ;; weirdness around the actual buffer file name of
      (find-file (concat "~/.emacs.d/init.el"))
    (find-file (concat "~/.emacs.d/init.org"))))




(defun add-hook-to-modes (modes hook)
  (dolist (mode modes)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
	      hook)))

(defun halt ()
  (interactive)
  (save-some-buffers t)
  (kill-emacs))
(defun my-whitespace-mode-hook ()
  (setq whitespace-action '(auto-cleanup)
	whitespace-style  '(face tabs trailing lines-tail empty)
	;; use fill-column value instead
	whitespace-line-column nil)
  (whitespace-mode))

(defun my-makefile-mode-hook ()
  (setq indent-tabs-mode t
	tab-width 4))

(defun make-region-read-only (start end)
  (interactive "*r")
  (let ((inhibit-read-only t))
    (put-text-property start end 'read-only t)))

(defun make-region-read-write (start end)
  (interactive "*r")
  (let ((inhibit-read-only t))
    (put-text-property start end 'read-only nil)))

(defun swap-buffers ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
	 (other (next-window))
	 (this-buffer (window-buffer this))
	 (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    )
  (other-window 1) ;;keep focus on starting window
  )

(defun prev-window ()
  (interactive)
  (other-window -1))

(defun kill-other-buffer ()
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window 1)
  )

(defun my-clipboard-to-elfeed ()
  (interactive)
  (let ((link (pbpaste)))
    (elfeed-add-feed link)))

(defun marker-is-point-p (marker)
  "test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun push-mark-maybe ()
  "push mark onto `global-mark-ring' if mark head or tail is not current location"
  (if (not global-mark-ring) (error "global-mark-ring empty")
    (unless (or (marker-is-point-p (car global-mark-ring))
		(marker-is-point-p (car (reverse global-mark-ring))))
      (push-mark))))


(defun backward-global-mark ()
  "use `pop-global-mark', pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark))

(defun forward-global-mark ()
  "hack `pop-global-mark' to go in reverse, pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (setq global-mark-ring (nreverse global-mark-ring))
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark)
  (setq global-mark-ring (nreverse global-mark-ring)))

;; (defun which-key-this-buffer ()
;; (interactive)
;; (which-key-show-keymap   major-mode))
;;

;; Automatically save on loss of focus.
(defun save-all ()
  "Save all file-visiting buffers without prompting."
  (interactive)
  (save-some-buffers t) ;; Do not prompt for confirmation.
  )
;; Automatically save all file-visiting buffers when Emacs loses focus.
(add-hook 'focus-out-hook 'save-all)
;;(add-hook 'focus-out-hook '(lambda () (message "ran focus out hook")))

(defun delete-window-balance ()
  "Delete window and rebalance the remaining ones."
  (interactive)
  (delete-window)
  (balance-windows))
(defun split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun vertical-horizontal-swizzle ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun not-anymore ()
  "not any more"
  (interactive)
  (message "not anymore")
  )

(defun prose-mode ()
  (interactive)
  (display-line-numbers-mode 0)
  (writeroom-mode 1)
  (page-break-lines-mode 1)
  (flyspell-mode 1)
  (electric-quote-mode 1)
  (abbrev-mode 1)
  (toggle-word-wrap 1)
  (setq buffer-face-mode-face
	'(:family "Times New Roman"
		  :height 180
		  :width semi-condensed))
  (buffer-face-mode)
  )

(defun chuck-into-next-weekish ()
  "sloppily reschedule current item into next week"
  (interactive)
  (org-schedule nil (format "+%dd"(+ 11 (random 9))) )
  (org-set-tags ":chucked:")
  )

;;taken from: https://www.reddit.com/r/emacs/comments/98w150/yet_another_emacs_convert/e4kf1y3/
;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(defun open-in-webstorm ()
  (interactive "")
  (shell-command (format  "webstorm --line %s --column %s %s" (line-number-at-pos) (current-column)  (buffer-file-name))))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq mode major-mode)
          (push buf buffer-mode-matches))))
    buffer-mode-matches))


(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defun revert-this-buffer ()
  (interactive)
  (revert-buffer nil t t)
  (message "reverted %s" (buffer-name)))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))


;; taken from https://emacsredux.com/blog/2013/06/15/open-line-above/
(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;;taken from https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode


(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate (arg)
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive "P")
  (exchange-point-and-mark)

  (unless arg (deactivate-mark nil))
  )

(defun full-frame-irregardless ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth))

    (full-frame-irregardless)

(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

(defun hs-cycle (&optional level)
  (interactive "p")
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           ;; TODO: Fix this case. `hs-show-block' needs to be
           ;; called twice to open all folds of the parent
           ;; block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
               (hs-hide-block)
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

(defun hs-global-cycle ()
  (interactive)
  (pcase last-command
    ('hs-global-cycle
     (save-excursion (hs-show-all))
     (setq this-command 'hs-global-show))
    (_ (hs-hide-all))))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mac-control-modifier 'control)
  (setq mac-function-modifier 'hyper)
  (setq mac-pass-command-to-system nil)
  (defun toggle-fullscreen ()
    "Toggle full screen"
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
  (defun pbcopy ()
    (interactive)
    (call-process-region (point) (mark) "pbcopy")
    (setq deactivate-mark t))

  (defun pbpaste ()
    (interactive)
    (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

  (defun pbcut ()
    (interactive)
    (pbcopy)
    (delete-region (region-beginning) (region-end)))

  (global-set-key (kbd "C-x M-c") 'pbcopy)
  (global-set-key (kbd "C-x M-v") 'pbpaste)
  (global-set-key (kbd "C-x M-x") 'pbcut)

  ;;recomended by brew
  (let ((default-directory "/opt/homebrew/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))

  (use-package exec-path-from-shell :ensure t
    :init
    (exec-path-from-shell-initialize))

  )

;;emacs style


(global-set-key (kbd "<f1>")  'eat)
(global-set-key (kbd "<f2>")  'magit-status)
(global-set-key (kbd "<f5>")  'mu4e)
(global-set-key (kbd "<f6>")  'org-agenda)
(global-set-key (kbd "<f7>")  'rg)
(global-set-key (kbd "<f8>")  'compile)
(global-set-key (kbd "<f9>")  'speed-type-top-1000)
(global-set-key (kbd "C-x M-r")  'revert-this-buffer )
(global-set-key (kbd "C-<f2>") 'multi-occur-in-this-mode)
(global-set-key (kbd "C-h C-/") 'which-key-show-major-mode)
(global-set-key (kbd "C-x ,") 'edit-init-org-file)
(global-set-key (kbd "C-x <f2>") 'open-in-webstorm)
(global-set-key (kbd "C-x <f5>") 'toggle-dark-light-state)
(global-set-key (kbd "C-x C-,") 'load-init-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'halt)
(global-set-key (kbd "C-x d") 'dired-jump)
(global-set-key (kbd "C-x C-r") 'recentf)
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x m")  'mu4e-compose-new)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-F") 'forward-word)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-o") 'smart-open-line)
(global-set-key (kbd "C-S-o") 'smart-open-line-above)

(global-set-key (kbd "s-SPC") 'cycle-spacing)

(global-set-key (kbd "C-<tab>") 'hs-cycle)
(global-set-key (kbd "C-S-<tab>") 'hs-cycle)




(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(global-set-key (kbd "C-c M-t") 'swap-buffers)
(global-set-key (kbd "C-x M-t") 'vertical-horizontal-swizzle)
(global-set-key (kbd "C-=") 'er/expand-region)


(global-set-key (kbd "C-z") 'evil-mode)
(define-key evil-normal-state-map (kbd "C-z") 'evil-mode)
(global-set-key (kbd "M-z") 'zap-up-to-char)


(global-set-key (kbd "C-`") 'jump-to-mark)
(global-set-key (kbd "M-`") 'backward-global-mark)
(global-set-key (kbd "C-M-`") 'forward-global-mark)
(global-set-key (kbd "C-SPC") 'push-mark-no-activate)
(global-set-key (kbd "C-S-SPC") 'set-mark-command)

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(define-key evil-normal-state-map (kbd "M-y") nil)
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-motion-state-map (kbd "C-y") nil)
(define-key evil-motion-state-map (kbd "C-d") nil)

(global-set-key (kbd "C-h h") 'not-anymore)
(global-set-key (kbd "C-h C-a") 'not-anymore)

(set-register ?e (cons 'file "~/.emacs.d/init.org"))
(set-register ?o (cons 'file "~/org/main.org"))
(set-register ?i (cons 'file "~/org/inbox.org"))
(set-register ?n (cons 'file "~/org/notes.org"))
(set-register ?w (cons 'file "~/org/work.org"))
(set-register ?z (cons 'file "~/.zshrc"))
(set-register ?d (cons 'file "~/Projects/lisp/drogue/drogue.lisp"))
(set-register ?p (cons 'file "~/Projects/"))
;;  (set-register ?e (cons 'file "~/Dropbox/elfeed.org"))
(set-register ?s (cons 'file "~/org/stories/ideas.org"))
(set-register ?k (cons 'file "~/Projects/Builds/qmk_firmware/keyboards/ergodox_ez/keymaps/mohsenil85/keymap.c"))

(use-package humanoid-themes :ensure t)
(use-package ef-themes :ensure t)
(use-package standard-themes :ensure t)

(defun load-dark ()
  (load-theme 'ef-dark t)
  (setq dark-light-state :dark ))

(defun load-light ()
  (load-theme 'ef-light t)
  (setq dark-light-state :light ))

(defun reset-themes()
  (mapc #'disable-theme custom-enabled-themes))

(defun toggle-dark-light-state ()
  (interactive)
  (reset-themes)
  (if (eq dark-light-state :dark)
      (load-light)
    (load-dark)))

(defun init-themes ()
  (reset-themes)
  (load-light))

(init-themes)
