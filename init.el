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
 '(
   (lisp . t)
   (shell . t)
   (python . t)
   ))

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
  (setq
  evil-collection-want-unimpaired-p nil
   forge-add-default-bindings t)
  ;(evil-collection-init)
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

(use-package ag :ensure t :defer t :config (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't))

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

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  ;; :hook (prog-mode . copilot-mode)	;
  :config
  (progn
    (setq copilot-expansion-delay 0.2)
    (setq copilot-expansion-limit 500)
    (setq copilot-node-executable "/Users/logan.k.mohseni/.nvm/versions/node/v16.20.2/bin/node" )
    )

  :bind (("C-c M-f" . copilot-complete)
	 :map copilot-completion-map
	 ("C-g" . 'copilot-clear-overlay)
	 ("M-p" . 'copilot-previous-completion)
	 ("M-n" . 'copilot-next-completion)
	 ("<tab>" . 'copilot-accept-completion)
	 ("M-f" . 'copilot-accept-completion-by-word)
	 ("M-<return>" . 'copilot-accept-completion-by-line)))

(use-package dired
  :straight nil				;
  :bind (:map dired-mode-map 
	      (("`" . dired-toggle-read-only)
	       ("j" .  evil-next-line)
	       ("k" . evil-previous-line)
	       ( "-" .  dired-up-directory)
	       ("~" . (lambda ()(interactive) (find-alternate-file "~/")))
	       ("RET" . dired-find-file)
	       ("C-<return>" . dired-find-file-other-window) 
	       ("/" . evil-search-forward) 
	       )
	      )

  :init
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
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


(eval-after-load "evil-mode"
  '(progn
     (add-to-list 'evil-emacs-state-modes 'dired-mode)
     ))

(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

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
(use-package prettier
  :ensure t
  :diminish
  :config
  (global-prettier-mode))

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
  :defer t
  :init
  (require 'keyfreq)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)

  )

(use-package magit 
  :ensure t
  :defer t
  :config       
  (define-key transient-map (kbd "<escape>") 'transient-quit-one)
  (setq magit-save-repository-buffers 'dontask)
  )
(use-package forge
  :after magit
  :config 
	   (setq auth-sources '("~/.authinfo"))

	   
  )

(use-package better-defaults :ensure t :defer t )
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

(use-package projectile
  :ensure t
  :diminish
  :config
    (projectile-global-mode)
    (setq projectile-completion-system 'default)
    (setq projectile-create-missing-test-files t)
    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-sort-order 'recentf)
    (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))

  ;; (use-package counsel-projectile
  ;;   :ensure t
  ;;   :config
  ;;   (add-hook 'after-init-hook 'counsel-projectile-mode))
  )

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

(use-package sentence-navigation
  :ensure t
  :bind (:map evil-motion-state-map 
		((")" . sentence-nav-evil-forward)
		 ("(" . sentence-nav-evil-backward)
		 ("g(" . sentence-nav-evil-backward-end)
		 ("g)" . sentence-nav-evil-forward-end)))
  :config
  (progn
    (define-key evil-outer-text-objects-map "s" 'sentence-nav-evil-a-sentence)
    (define-key evil-inner-text-objects-map "s" 'sentence-nav-evil-inner-sentence))
  )

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

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

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

(use-package hotfuzz
  :init
  (setq completion-styles '(hotfuzz basic)
	completion-ignore-case t
read-buffer-completion-ignore-case t
read-file-name-completion-ignore-case t

	  ;; completion-category-defaults nil
	  ;; completion-category-overrides '((file (styles partial-completion)))
	))

(defvar lispular-modes-list
  'emacs-lisp-mode-hook
  'lisp-mode-hook)

(add-hook 'lisp-mode-hook
	    (lambda ()
	      (set (make-local-variable 'lisp-indent-function)
		   'common-lisp-indent-function)))


;; (use-package smartparens
;;   :ensure t
;;   ;;:diminish ""

;;   :init 
;;   (require 'smartparens-config)

;;   (unbind-key  "C-M-f" smartparens-strict-mode-map)
;;   (unbind-key  "C-M-b" smartparens-strict-mode-map)
;;   (unbind-key  "C-M-u" smartparens-strict-mode-map)
;;   (unbind-key  "C-M-d" smartparens-strict-mode-map)
;;   (unbind-key  "C-M-p" smartparens-strict-mode-map)
;;   (unbind-key  "C-M-n" smartparens-strict-mode-map)
;;   (unbind-key  "M-s"   smartparens-strict-mode-map)
;;   (unbind-key  "M-<up>" smartparens-strict-mode-map)
;;   (unbind-key  "M-<down>"  smartparens-strict-mode-map)
;;   (unbind-key  "M-r" smartparens-strict-mode-map)
;;   (unbind-key  "M-(" smartparens-strict-mode-map)
;;   (unbind-key  "C-)"  smartparens-strict-mode-map)
;;   (unbind-key  "C-<right>" smartparens-strict-mode-map)
;;   (unbind-key  "C-}" smartparens-strict-mode-map)
;;   (unbind-key  "C-<left>" smartparens-strict-mode-map)
;;   (unbind-key  "C-(" smartparens-strict-mode-map)
;;   (unbind-key  "C-M-<left>" smartparens-strict-mode-map)
;;   (unbind-key  "C-{" smartparens-strict-mode-map)
;;   (unbind-key  "C-M-<right>" smartparens-strict-mode-map)
;;   (unbind-key  "M-S" smartparens-strict-mode-map)
;;   (unbind-key  "M-j" smartparens-strict-mode-map)
;;   (unbind-key  "M-?" smartparens-strict-mode-map)

;;   (add-hook 'emacs-lisp-mode-hook #'evil-smartparens-mode)


;;   :config
;;   (general-define-key
;;    :keymaps 'smartparens-mode-map
;;    :prefix ","
;;    "s" 'sp-splice-sexp
;;    "w" 'sp-wrap-round
;;    "(" 'sp-wrap-round
;;    "[" 'sp-wrap-curly
;;    "{" 'sp-wrap-square
;;    "o" 'sp-split-sexp
;;    "j" 'sp-join-sexp
;;    "r" 'sp-raise-sexp
;;    )
;;   (general-define-key
;;    :keymaps 'smartparens-mode-map
;;    :states '(normal visual motion)
;;    ")" 'sp-forward-sexp
;;    "(" 'sp-backward-sexp
;;    "C-(" 'sp-up-sexp
;;    "C-)" 'sp-down-sexp
;;    ">" 'sp-forward-slurp-sexp
;;    "<" 'sp-backward-slurp-sexp
;;    "C->" 'sp-forward-barf-sexp
;;    "C-<" 'sp-backward-barf-sexp
;;    )

;;   (use-package evil-smartparens
;;     :ensure t
;;     :init 
;;     (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
;;   )

;; (use-package rainbow-delimiters
;;   :ensure t
;;   :diminish ""
;;   :config
;;   (progn
;;     (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
;;     (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
;;     (add-hook 'slime-repl-mode-hook #'rainbow-delimiters-mode)
;;     (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

;; 			   ;;;     (eval-after-load "slime"
;; 			   ;;;       '(progn
;; 			   ;;;          (define-key evil-normal-state-map (kbd "M-.") 'slime-edit-definition)
;; 			   ;;;          (define-key evil-normal-state-map (kbd "M-,") 'slime-pop-find-definition-stack)))
;; 			   ;;;
;;      (use-package slime
;;        ;;:defer 1
;;        ;;:load-path  "~/.emacs.d/vendor/slime"
;;        :config (progn

;; 		 (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; 		 (setq inferior-lisp-program 
;; 		       "/usr/local/bin/sbcl --noinform --no-linedit")
;; 		 (require 'slime-autoloads)
;; 		 (add-to-list 'load-path "~/.emacs.d/vendor/slime/contrib")
;; 		 (setq slime-contribs 
;; 		       '(slime-fancy 
;; 			 slime-asdf 
;; 					     ;slime-banner
;; 			 slime-indentation
;; 			 slime-quicklisp 
;; 			 slime-xref-browser
;; ))
;; 		 (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
;; 		 (slime-setup))

;;        :bind (
;; 	      ;; ("C-c s" . slime-selector)
;; 	      ;; ("M-." . slime-edit-definition)
;; 	      ;; ("M-," . slime-pop-definition-stack)
;; 	      )
;;        )

;;      (require 'info-look)

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

(use-package tide
 :ensure t
 :after (company flycheck)
 :hook ((typescript-ts-mode . tide-setup)
        (tsx-ts-mode . tide-setup)
        (typescript-ts-mode . tide-hl-identifier-mode)
        (before-save . tide-format-before-save))) 

(use-package jest-test-mode 
:ensure t 
:commands jest-test-mode
:hook (typescript-mode js-mode typescript-tsx-mode))

;;taken from https://macowners.club/posts/email-emacs-mu4e-macos/#storing-trusted-root-certificates


(use-package mu4e
  :straight nil
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/"
  :config
  (require 'mu4e-contrib)
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
   mue4e-headers-skip-duplicates  t


   mu4e-maildir       "~/Maildir/gmail"   ;; top-level Maildir
   mu4e-refile-folder "/gmail/Archive"
   mu4e-sent-folder   "/gmail/[Gmail]/Sent Mail"
   mu4e-drafts-folder "/gmail/[Gmail]/Drafts"
   mu4e-trash-folder  "/gmail/[Gmail]/Trash"
   )

  (setq   mu4e-maildir-shortcuts
  	  '(("/gmail/INBOX" . ?i)
       	    ("/gmail/[Gmail]/Sent Mail" . ?I)
       	    ("/example/INBOX" . ?e)
       	    ("/example/Sent" . ?E))
          )

  (require 'smtpmail)

               (mu4e-bookmark-define
     		"maildir:/gmail/INBOX"
     		"Inbox - Gmail"
     		?g)
               (mu4e-bookmark-define
     		"maildir:/example/INBOX"
     		"Inbox - example"
     		?e)
  
  )

(use-package emacs
  :init
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

  ;; Enable recursive minibuffers

    (blink-cursor-mode -1)
    (defalias 'yes-or-no-p 'y-or-n-p)
    (delete-selection-mode 1)
    (electric-pair-mode 1)
    (global-display-line-numbers-mode 1)
    (global-visual-line-mode t)
    (menu-bar-mode 0)
    (prefer-coding-system 'utf-8)
    (recentf-mode 1)
    (scroll-bar-mode 0)
    (server-start)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8-unix)
    (tool-bar-mode 0)
    (tooltip-mode -1)

    (setq
     auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
     auto-save-visited-interval 1
     auto-save-visited-mode 1
     backup-directory-alist `((".*" . ,temporary-file-directory))
     confirm-kill-processes nil
     confirm-nonexistent-file-or-buffer nil
     default-fill-column 80		; toggle wrapping text at the 80th character
     delete-old-versions t 		; delete excess backup versions silently
     enable-recursive-minibuffers t
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
     split-width-threshold 160
     switch-to-buffer-preserve-window-point t
     tab-always-indent 'complete 
     tooltip-use-echo-area t
     use-dialog-box nil
     user-full-name "Logan Mohseni"
     user-mail-address "mohsenil85@gmail.com"
     vc-follow-symlinks t 				       ; don't ask for confirmation when opening symlinked file
     vc-make-backup-files t 		; make backups file even when in version controlled dir
     version-control t 		; use version control
     visible-bell t
     )

)

  


  (defun zsh-shell-mode-setup ()
    (setq-local comint-process-echoes t))
  (add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

    (require 'uniquify)
    (setq uniquify-buffer-name-style 'forward)
    (winner-mode 1)

;;bigger font size for my poor old aching occulars
(setq default-frame-alist  '(
				      (font . "-*-Monaco-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"   )
				      (height . 36)
				      (width . 136)
				      (top . 50)
				      (left . 30)
				      ))

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

(defun foobl (ak)
  (let ((foo 'bar)
	(zip 'ping)
	))
  (print foo))

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

  (linum-mode 0)
  (writeroom-mode 1)
  (page-break-lines-mode 1)
  (flyspell-mode 1)
  (electric-quote-mode 1)
  (abbrev-mode 1)
  (word-wrap-mode 1)
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
  (org-set-tags-to ":chucked:")
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
    :config 
    (exec-path-from-shell-initialize))

  )

(when (eq system-type 'gnu/linux)
  (global-set-key (kbd "M-V") 'mouse-yank-primary)
  (global-set-key (kbd "M-v") 'evil-paste-after)
  (global-set-key (kbd "M-c") 'evil-yank)
  (global-set-key (kbd "M-X") 'evil-delete-char)

  )
					  ;  (when (string= (system-name) "zig") 
					  ;(set-frame-font "Inconsolata-16")
					  ;)

(define-key evil-normal-state-map (kbd "M-y") nil)
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-motion-state-map (kbd "C-y") nil)
(define-key evil-motion-state-map (kbd "C-d") nil)

;;emacs style

  (global-set-key (kbd "C-c m")  'mu4e)
  (global-set-key (kbd "<f1>")  '(lambda()(interactive)(term "zsh")))
  (global-set-key (kbd "<f2>")  'rgrep)
  (global-set-key (kbd "<f6>")  'revert-this-buffer )
  (global-set-key (kbd "C-<f2>") 'multi-occur-in-this-mode)
  (global-set-key (kbd "C-h C-/") 'which-key-show-major-mode)
  ;; (global-set-key (kbd "C-s") 'swiper)  ;;search in file;;swiper?
  (global-set-key (kbd "C-x ,") 'edit-init-org-file)
  (global-set-key (kbd "C-x <f2>") 'open-in-webstorm)
  (global-set-key (kbd "C-x <f5>") 'toggle-dark-light-state)
  (global-set-key (kbd "C-x C-,") 'load-init-file)
  ;; (global-set-key (kbd "C-x C-a") 'mark-whole-buffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "C-x C-c") 'halt)
  (global-set-key (kbd "C-x C-j") 'dired-jump)
  (global-set-key (kbd "C-x C-k") 'kill-this-buffer)
  (global-set-key (kbd "C-x C-r") 'recentf)
  (global-set-key (kbd "C-x M-t") 'vertical-horizontal-swizzle)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x m")  'mu4e-compose-new)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "M-0") 'delete-window-balance)
  ;; (global-set-key (kbd "M-1") 'delete-other-windows)
  ;; (global-set-key (kbd "M-2") 'split-window-below-focus)
  ;; (global-set-key (kbd "M-3") 'split-window-right-focus)
  (global-set-key (kbd "M-o") 'other-window)
  (global-set-key (kbd "C-c M-t") 'swap-buffers)
  ;;(global-set-key (kbd "M-x") 'counsel-M-x) ;;M-x
    (global-set-key (kbd "C-M-x") 'evil-mode)
  (global-set-key (kbd "M-z") 'zap-up-to-char)
    (global-set-key (kbd "C-z") 'undo)


;;  (define-key evil-normal-state-map (kbd "-") 'dired-jump)

  ;; (define-key evil-normal-state-map (kbd "C-n") 'other-window)
  ;; (define-key evil-normal-state-map (kbd "C-p") 'prev-window)

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
(set-register ?e (cons 'file "~/Dropbox/elfeed.org"))
(set-register ?s (cons 'file "~/org/stories/ideas.org"))
(set-register ?k (cons 'file "~/builds/qmk_firmware/keyboards/ergodox_ez/keymaps/mohsenil85/keymap.c"))

;; (use-package gandalf-theme :ensure t)
;; (use-package mbo70s-theme :ensure t)
;; (use-package warm-night-theme  :ensure t )
;; (use-package twilight-bright-theme :ensure t )
;; (use-package standard-themes :ensure t )
;; (use-package alect-themes :ensure t)
;; (use-package yoshi-theme :ensure t)
;; (use-package slime-theme :ensure t)
;; (use-package basic-theme :ensure t)
;; (use-package minimal-theme :ensure t)
;; (use-package white-theme :ensure t)
;; (use-package paper-theme :ensure t )
;; (use-package django-theme :ensure t )
;; (use-package solarized-theme :ensure t)
;; (use-package minimal-theme :ensure t)


(use-package humanoid-themes :ensure t)
(use-package soft-morning-theme :ensure t )
(use-package soft-charcoal-theme :ensure t )
(use-package hydandata-light-theme :ensure t)
(use-package standard-themes :ensure t )

(defun load-dark ()
  (load-theme 'humanoid-dark t)
  (setq dark-light-state :dark ))
;; (defun load-dark ()
;;   (load-theme 'humanoid-dark t)
;;   (setq dark-light-state :dark ))

(defun load-light ()
  (load-theme 'humanoid-light t)
  (setq dark-light-state :light ))
;; (defun load-light ()
;;   (load-theme 'hydandata-light t)
;;   (setq dark-light-state :light ))

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

;; (load-file "~/.emacs.d/secrets.el.gpg")

(use-package minions :ensure t :bind ("<S-down-mouse-3>" . #'minions-minor-modes-menu))
