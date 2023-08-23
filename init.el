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
(setq straight-use-package-by-default t)

(use-package no-littering :ensure t)
;; (load-file "~/.emacs.d/org-init.el")


;; ;;used in debugging
;; ;;(use-package evil :ensure t :config (evil-mode 1))
;; ;;(setq debug-on-error t)

;; ;;(use-package org :ensure t)	

;; ;;(straight-use-package 'org)
;; (require 'ob-tangle)


;; (org-babel-load-file
;;  (expand-file-name "emacs-init.org"
;; 		   user-emacs-directory))

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


(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (lisp . t)
   (shell . t)
   (python . t)
   ))

;  (evil-org-mode 1)

(use-package diminish :ensure t)

;;done in the earlier init.el
;;(use-package evil :ensure t :config (evil-mode 1))

(use-package evil
  :ensure t
  :init
  ;;turn on for evil collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)

  )

(use-package evil-collection
  :after evil
  :diminish
  :ensure t
  :config
  (setq evil-collection-want-unimpaired-p nil)
  (evil-collection-init)
  )



;; (use-package evil-visual-mark-mode
;;   :ensure t
;;   :config
;;   (evil-visual-mark-mode 1))

(use-package ag :ensure t :defer t :config (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't))

(use-package bind-key
  ;; A simple way to manage personal keybindings, provided by `use-package'
  :init
  ;; If non-nil, extract docstrings from lambdas, closures and keymaps if possible.
  (setq bind-key-describe-special-forms t)
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

;;re-write using M-.
;; (use-package dumb-jump
;; :ensure t 
;; :defer t 
;;   :bind (("M-g o" . dumb-jump-go-other-window)
;; 	 ("M-g j" . dumb-jump-go)
;; 	 ("M-g x" . dumb-jump-go-prefer-external)
;; 	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
;;   :config (setq dumb-jump-selector 'helm)
;;   )

(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

;; (use-package elfeed
;;   :ensure t
;;   ;;:defer t
;;   :commands (elfeed-search-mode elfeed-show-mode)
;;   :init (progn
;; 	  ;; (use-package elfeed-goodies :ensure t)
;; 	  (use-package elfeed-org :ensure t))
;;   :config
;;   (progn
;;     (require 'elfeed-goodies)
;;     (elfeed-goodies/setup)
;;     (setq 
;;      elfeed-goodies/powerline-default-separator nil
;;      elfeed-goodies/entry-pane-position 'bottom)

;;     (setq elfeed-db-directory "~/.emacs.d/elfeed")
;;     (require 'elfeed-org)
;;     (elfeed-org)
;;     (setq rmh-elfeed-org-files (list "~/Dropbox/elfeed.org")))
;;     )

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

;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (typescript-mode . lsp)
;;          (javascript-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; ;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; ;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; ;;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; ;; optionally if you want to use debugger
;; ;;(use-package dap-mode :straight nil)
;; ;;(use-package dap-typescript) to load the dap adapter for your language

;; ;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))

(use-package magit 
  :ensure t
  :defer t
  :config       
  (define-key transient-map (kbd "<escape>") 'transient-quit-one)
  (setq magit-save-repository-buffers 'dontask)
  )

(use-package better-defaults :ensure t :defer t )
(use-package bind-map :ensure t :defer t)
(use-package emojify :ensure t :defer t )
(use-package markdown-mode :ensure t :defer t)
(use-package smex :ensure t :defer t)
(use-package feebleline :ensure t :defer t)
(use-package fzf :ensure t :defer t)
;;(use-package recursive-narrow :ensure t :defer t)

;; (use-package org-brain :ensure t
;;   :init
;;   (setq org-brain-path "~/Dropbox/org-brain/")
;;   ;; For Evil users
;;   (with-eval-after-load 'evil
;;     (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
;;   :config
;;   (setq org-id-track-globally t)
;;   (setq org-id-locations-file "~/.emacs.d/org-id-locations")
;;   (push '("b" "Brain" plain (function org-brain-goto-end)
;; 	  "* %i%?" :empty-lines 1)
;; 	org-capture-templates)
;;   (setq org-brain-visualize-default-choices 'all)
;;   (setq org-brain-title-max-length 16))

(use-package page-break-lines
  :diminish ""
  :ensure t 
  :defer t 
  :config (global-page-break-lines-mode))

(use-package persistent-scratch :ensure t
  :config (persistent-scratch-setup-default))

(use-package powerline 
    :ensure
    :config (progn
              (require 'powerline)
              (powerline-center-evil-theme)
))

(use-package projectile
  :ensure t
  :diminish
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-completion-system 'ivy)
    (setq projectile-create-missing-test-files t)
    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-sort-order 'recentf)
    (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
    )
  (use-package counsel-projectile
    :ensure t
    :config
    (add-hook 'after-init-hook 'counsel-projectile-mode))
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

;;taken from https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
;; (use-package avy :ensure t :commands (avy-goto-word-1))

(use-package ivy 
  :ensure  t ;ivy-hydra 
  :diminish ivy-mode ; does not display ivy in the modeline
  :bind (:map ivy-mode-map  ; bind in the ivy buffer
		("C-'" . ivy-avy)) ; C-' to ivy-avy
  :config
  (ivy-mode 1)        ; enable ivy globally at startup
					  ;
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	  ;; allow input not in order
	  '((t   . ivy--regex-fuzzy)
	    (t   . ivy--regex-ignore-order))))

;; (use-package ivy :demand
;;   :config
;;   (setq ivy-use-virtual-buffers t
;; 	ivy-count-format "%d/%d "))

(use-package counsel :ensure t )
(use-package swiper :ensure t )
(use-package ivy-hydra :ensure t :defer t)

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

;; ;;
;; ;; NOTE: USE `lsp_mode` not `eglot`.
;; ;;
;; ;;       `eglot` is only useful if you want to manage `python lsp_server.py`
;; ;;       yourself for debugging purposes
;; ;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; EGLOT CONFIGURATION
;; ;;
;; ;;   add to init.el
;; ;;
;; ;; NOTE:
;; ;;
;; ;; `eglot` allows you to separately run the server yourself, which can be useful
;; ;; for debugging. `lsp-mode` requires that it launches the process, and then
;; ;; your logs are buried in an emacs buffer somewhere.  If you want to run it
;; ;; yourself:
;; ;;
;; ;;     python lsp_server.py
;; ;;
;; ;; And then just make sure the `lsp_port` you run on is the same one mentioned
;; ;; at the bottom of this config.


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; LLM Mode
;; ;;
;; ;; This is only useful at the current stage of development, and should be
;; ;; removed once `lsp-mode` is integrated and allows concurrent LSPs per each
;; ;; buffer.

;; (define-derived-mode llm-mode fundamental-mode "llm"
;;   "A mode for llm files."
;;   (setq-local comment-start "#")
;;   (setq-local comment-start-skip "#+\\s-*"))

;; (defvar llm-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     map)
;;   "Keymap for `llm-mode'.")

;; (defvar llm-mode-hook nil)

;; (provide 'llm-mode)

;; (add-to-list 'auto-mode-alist '("\\.llm\\'" . llm-mode))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; LLM Mode (TODO REDUNDANT)

;; (add-to-list 'load-path (expand-file-name "~/_/uniteai/"))

;; (require 'llm-mode)

;; (use-package llm-mode
;;   :straight nil
;;   :ensure nil
;;   :mode ("\\.llm\\'" . llm-mode)
;;   :hook (llm-mode . eglot-ensure))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; EGlot

;; (use-package eglot
;;   :straight nil
;;   :ensure t
;;   :hook
;;   (eglot--managed-mode . company-mode)
;;   :init
;;   ;; Tell eglot not to ask if you're ok with the server modifying the document.
;;   (setq eglot-confirm-server-initiated-edits nil)
;;   :config
;;   (define-key eglot-mode-map (kbd "M-'") 'eglot-code-actions)
;;   )


;; ;;;;;;;;;;

;; ;; Global stopping
;; (defun eglot-stop ()
;;   (interactive)
;;   (let* ((server (eglot--current-server-or-lose))
;;          (doc (eglot--TextDocumentIdentifier)))
;;     (eglot-execute-command server 'command.stop (vector doc))))

;; ;; Example Counter
;; (defun eglot-example-counter ()
;;   (interactive)
;;   (let* ((server (eglot--current-server-or-lose))
;;          (doc (eglot--TextDocumentIdentifier))
;;          (pos (eglot--pos-to-lsp-position (point))))
;;     (eglot-execute-command server 'command.exampleCounter (vector doc pos))))

;; ;; Local LLM
;; (defun eglot-local-llm ()
;;   (interactive)
;;   (unless mark-active
;;     (error "No region selected"))
;;   (let* ((server (eglot--current-server-or-lose))
;;          (doc (eglot--TextDocumentIdentifier))
;;          (range (list :start (eglot--pos-to-lsp-position (region-beginning))
;;                       :end (eglot--pos-to-lsp-position (region-end)))))
;;     (eglot-execute-command server 'command.localLlmStream (vector doc range))))

;; ;; Transcription
;; (defun eglot-transcribe ()
;;   (interactive)
;;   (let* ((server (eglot--current-server-or-lose))
;;          (doc (eglot--TextDocumentIdentifier))
;;          (pos (eglot--pos-to-lsp-position (point))))
;;     (eglot-execute-command server 'command.transcribe (vector doc pos))))

;; ;; OpenAI
;; (defun eglot-openai-gpt ()
;;   (interactive)
;;   (unless mark-active
;;     (error "No region selected"))
;;   (let* ((server (eglot--current-server-or-lose))
;;          (doc (eglot--TextDocumentIdentifier))
;;          (range (list :start (eglot--pos-to-lsp-position (region-beginning))
;;                       :end (eglot--pos-to-lsp-position (region-end)))))
;;     (eglot-execute-command server 'command.openaiAutocompleteStream (vector doc range "FROM_CONFIG_COMPLETION" "FROM_CONFIG"))))

;; (defun eglot-openai-chatgpt ()
;;   (interactive)
;;   (unless mark-active
;;     (error "No region selected"))
;;   (let* ((server (eglot--current-server-or-lose))
;;          (doc (eglot--TextDocumentIdentifier))
;;          (range (list :start (eglot--pos-to-lsp-position (region-beginning))
;;                       :end (eglot--pos-to-lsp-position (region-end)))))
;;     (eglot-execute-command server 'command.openaiAutocompleteStream (vector doc range "FROM_CONFIG_CHAT" "FROM_CONFIG"))))

;; (add-hook 'llm-mode-hook
;;           (lambda ()
;;             (define-key llm-mode-map (kbd "C-c l s") 'eglot-stop)

;;             (define-key llm-mode-map (kbd "C-c l e") 'eglot-example-counter)

;;             (define-key llm-mode-map (kbd "C-c l l") 'eglot-local-llm)

;;             (define-key llm-mode-map (kbd "C-c l v") 'eglot-transcribe)

;;             (define-key llm-mode-map (kbd "C-c l g") 'eglot-openai-gpt)
;;             (define-key llm-mode-map (kbd "C-c l c") 'eglot-openai-chatgpt)
;;             (eglot-ensure)))

;; (require 'eglot)
;; (add-to-list 'eglot-server-programs
;;              `(llm-mode . ("localhost" 5033)))

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
  (show-paren-mode 1)
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
   explicit-shell-file-name "/bin/zsh"
   explicit-zsh-args '("--login" "--interactive")
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


(defun zsh-shell-mode-setup ()
  (setq-local comint-process-echoes t))
(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
  (winner-mode 1)

  ;; (set-frame-font "-*-Monaco-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")

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
  (if (string= buffer-file-name  (expand-file-name ".emacs.d/emacs-init.org" "~") ) ;; weirdness around the actual buffer file name of 
      (find-file (concat "~/.emacs.d/init.el"))
    (find-file (concat "~/.emacs.d/emacs-init.org"))))




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

(eval-after-load "evil-mode"	'(progn
				     (add-to-list 'evil-normal-state-modes 'package-menu-mode)

				     ))

(general-define-key
 :keymaps '(package-menu-mode-map )
 "SPC" 'evil-scroll-page-down
 "DEL" 'evil-scroll-page-up
 "I" 'package-menu-mark-install
 "X" 'package-menu-execute          	  
 "D" 'package-menu-mark-delete
 ;;"u" 'package-menu-mark-unmark
 "RET" 'package-menu-describe-package 
 "r" 'package-menu-refresh
 )

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
  ""
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
  ;; (let ((default-directory "/opt/homebrew/share/emacs/site-lisp/"))
  ;;   (normal-top-level-add-subdirs-to-load-path))

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

;; (use-package mlscroll
;;   :ensure t
;;   :config
;;   (setq mlscroll-shortfun-min-width 11) ;truncate which-func, for default mode-line-format's
;;   (mlscroll-mode 1))

;; (add-to-list 'auto-mode-alist '("\\.json" . js-mode))
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; 					  ;(use-package ac-js2 :ensure)

;; (defun my-js-mode-stuff ()
;;   (setq js2-highlight-level 3)
;;   (define-key js-mode-map "{" 'paredit-open-curly)
;;   (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
;;   )
;; (use-package js2-mode :ensure t :defer t :config (my-js-mode-stuff))

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))

;; (use-package tide :ensure t :defer t :config
;;   ;; aligns annotation to the right hand side
;;   (setq company-tooltip-align-annotations t)

;;   ;; formats the buffer before saving
;;   (add-hook 'before-save-hook 'tide-format-before-save)

;;   (add-hook 'typescript-mode-hook #'setup-tide-mode)
;;   )

;;from http://cachestocaches.com/2015/8/c-completion-emacs/
					  ; (use-package irony
					  ;   :ensure t
					  ;   :defer t
					  ;   :init
					  ;   (add-hook 'c++-mode-hook 'irony-mode)
					  ;   (add-hook 'c-mode-hook 'irony-mode)
					  ;   (add-hook 'objc-mode-hook 'irony-mode)
					  ;   :config
					  ;   ;; replace the `completion-at-point' and `complete-symbol' bindings in
					  ;   ;; irony-mode's buffers by irony-mode's function
					  ;   (defun my-irony-mode-hook ()
					  ;     (define-key irony-mode-map [remap completion-at-point]
					  ;       'irony-completion-at-point-async)
					  ;     (define-key irony-mode-map [remap complete-symbol]
					  ;       'irony-completion-at-point-async))
					  ;   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
					  ;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
					  ;   )

					  ; ;; == company-mode ==
					  ; (use-package company
					  ;   :ensure t
					  ;   :defer t
					  ;   :init (add-hook 'after-init-hook 'global-company-mode)
					  ;   :config
					  ;   (use-package company-irony :ensure t :defer t)
					  ;   (setq company-idle-delay              nil
					  ; 	company-minimum-prefix-length   2
					  ; 	company-show-numbers            t
					  ; 	company-tooltip-limit           20
					  ; 	company-dabbrev-downcase        nil
					  ; 	company-backends                '((company-irony company-gtags))
					  ; 	)
					  ;   :bind ("C-;" . company-complete-common)
					  ;   )

;; (use-package meghanada 
;;   :ensure t
;;   :defer t
;;   :config 
;;   (progn
;;     (add-hook 'java-mode-hook
;; 		(lambda ()
;; 		  ;; meghanada-mode on
;; 		  (meghanada-mode t)
;; 		  (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
;;     )
;;   )
;; (use-package emacs-eclim :ensure
;;   :config
;;   (progn
;;     (require 'eclim)
;;     (global-eclim-mode)
;;     (setf eclim-eclipse-dirs
;;           '("/Users/lmohseni/java-neon/Eclipse.app/Contents/Eclipse"))
;;     (setf eclim-executable 
;;           '("/Users/lmohseni/java-neon/Eclipse.app/Contents/Eclipse/eclim"))
;; (setq help-at-pt-display-when-idle t)
;; (setq help-at-pt-timer-delay 0.1)
;; (help-at-pt-set-timer)
;; ;; regular auto-complete initialization
;; (require 'auto-complete-config)
;; (ac-config-default)

;; ;; add the emacs-eclim source
;; (require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)
;;     ))

;; 	  (use-package helm
;; 	    :ensure t
;; 	    :diminish """~/.emacs/data/helm-history")


;; 	      (setq helm-M-x-fuzzy-match t)
;; 	      (setq helm-recentf-fuzzy-match t)
;; 	      (setq helm-mode-fuzzy-match t)
;; 	      (setq helm-buffers-fuzzy-matching t)
;; 	      (setq helm-ff-auto-update-initial-value t) ;; use c-backspact
;; 	      (setq helm-move-to-line-cycle-in-source t)


;; 	      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; 	      (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; 	      (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
;; 	      ;;(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
;; 	      (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history) 

;; 	      (autoload 'helm-descbinds      "helm-descbinds" t)
;; 	      (autoload 'helm-eshell-history "helm-eshell"    t)
;; 	      (autoload 'helm-esh-pcomplete  "helm-eshell"    t)

;; 	      (setq helm-autoresize-mode t)
;; 	      (setq helm-split-window-in-side-p t)
;; 	      (setq helm-ff-file-name-history-use-recentf t)
;; 	      (setq helm-autoresize-mode t)

;;       (global-set-key (kbd "C-c h g") 'helm-google-suggest)
;;       (global-set-key (kbd "C-c h r") 'helm-register)
;; 	      (helm-mode 1))
;; )

;; 	  (use-package helm-descbinds :ensure t :defer t :config
;; 	    (progn
;; 	      (require 'helm-descbinds)
;; 	      (helm-descbinds-mode)
;; 	      )
;; 	    )

;;(use-package helm-ag :ensure t :defer t)
;; (use-package helm-ack :ensure t :defer t)

;;     (use-package helm-gtags
;;       :ensure t
;;       :defer t
;;       :config
;;       (progn
;; 	;;; Enable helm-gtags-mode
;; 	(add-hook 'c-mode-hook 'helm-gtags-mode)
;; 	(add-hook 'c++-mode-hook 'helm-gtags-mode)
;; 	(add-hook 'asm-mode-hook 'helm-gtags-mode)
;; 	(add-hook 'java-mode-hook 'helm-gtags-mode)

;; 	;; customize
;; 	(setq
;; 	 helm-gtags-path-style 'relative)
;;   (setq  
;; 	 helm-gtags-auto-update t) 

;;       (setq helm-gtags-prefix-key "C-t")
;; ;      (helm-gtags-suggested-key-mapping t)
;; 	;; key bindings
;; 	(eval-after-load "helm-gtags"
;; 	  '(progn
;; 	     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
;; 	     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
;; 	     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
;; 	     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
;; 	     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;; 	     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;; 	     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))))


;;       )

;; (add-to-list 'load-path  "~/Projects/elisp/mu4e-multi/" )
;;       (require 'mu4e-multi)

;;  (use-package mu4e  
;;    ;;:load-path mu-load-path
;;    :commands (make-mu4e-context)
;;    :init (progn

;;	    (when (fboundp 'imagemagick-register-types)
;;	      (imagemagick-register-types))
;;	    )
;;    :config
;;    (progn
;;      (general-define-key
;;       :keymaps '(mu4e-view-mode-map mu4e-headers-mode-map)
;;       "SPC" 'evil-scroll-page-down
;;       "DEL" 'evil-scroll-page-up)
;;      )
;;    :bind ("C-c m" . mu4e)
;;    ))
;;
;;;;

;;    (use-package el-get :ensure t
;;    :config

;;  (require 'el-get)
;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;; (el-get 'sync)
;; )

;;add the vendor directory to the load path
;;hopefully obsoleted by use of quelpa
;;	(let ((default-directory "~/.emacs.d/vendor/"))
;;  (normal-top-level-add-subdirs-to-load-path))

;;;(setq default-directory (getenv "HOME"))

;; (setq browse-url-browser-function 'eww-browse-url)

;; (add-hook 'text-mode-hook 'flyspell-mode)

;;;	  (add-hook 'text-mode-hook #'abbrev-mode)




					  ;    ;(add-hook 'text-mode-hook 'writeroom-mode)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;(put 'narrow-to-page 'disabled nil)

;;;(transient-mark-mode t)

;;bigger font size for my poor old aching occulars
     (add-to-list 'default-frame-alist '(font . "-*-Monaco-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"   ))

;(setq initial-buffer-choice "~/org")
;(setq initial-buffer-choice "~/org/organizer.org")

;(setq initial-buffer-choice  (bookmark-bmenu-list))

;  (org-agenda-list)
;  (delete-other-windows)
;  (org-agenda-day-view)

;      (use-package auto-complete
;        :ensure
;        :config
;        (progn
;          (require 'auto-complete-config)
;          (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict/")
;          (ac-config-default)
;          (ac-set-trigger-key "TAB")
;          (ac-set-trigger-key "<tab>")
; ))

;  (use-package deft 
;    :ensure t
;    :config (progn
;              (setq
;               deft-extension "org"
;               deft-directory "~/org/"
;               deft-text-mode 'org-mode)
;              (global-set-key (kbd "H-d") 'deft)))

;;   (use-package evil-evilified-state
;;   :load-path "~/Projects/elisp/evil-evilified-state/")

;; (use-package evil-org
;;   :ensure t
;;   :after org
;; :diminish
;;   :defer t
;;   :init (require 'evil-org)
;;   :hook (org-mode . (lambda () evil-org-mode))
;;  ; :config (evil-org-mode 1)
      ;; )  
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (evil-org-mode))
  :config
  (require 'evil-org-agenda)
)

;;;      (use-package evil-rsi 
;;;	 :diminish ""
;;;	 :ensure t 
;;;	 :defer t 
;;;	 :config (evil-rsi-mode 1) )

;;  (use-package exwm :ensure t :defer t 
;;    :config (exwm-config-default))

;(use-package flx :ensure t)

;;      (use-package helm-swoop 
;;        :ensure
;;        :config
;;        (progn
;;          (global-set-key (kbd "M-i") 'helm-swoop)
;;          (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;;          (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;;          (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
;;
;;          ;; When doing isearch, hand the word over to helm-swoop
;;          (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;;          ;; From helm-swoop to helm-multi-swoop-all
;;          (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;;          ;; When doing evil-search, hand the word over to helm-swoop
;;          ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)
;;
;;          ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
;;          (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)
;;
;;          ;; Move up and down like isearch
;;          (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
;;          (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
;;          (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
;;          (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
;;
;;          ;; Save buffer when helm-multi-swoop-edit complete
;;          (setq helm-multi-swoop-edit-save t)
;;
;;          ;; If this value is t, split window inside the current window
;;          (setq helm-swoop-split-with-multiple-windows nil)
;;
;;          ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
;;          (setq helm-swoop-split-direction 'split-window-vertically)
;;
;;          ;; If nil, you can slightly boost invoke speed in exchange for text color
;;          (setq helm-swoop-speed-or-color nil)
;;
;;          ;; ;; Go to the opposite side of line from the end or beginning of line
;;          (setq helm-swoop-move-to-line-cycle t)
;;
;;          ;; Optional face for line numbers
;;          ;; Face name is `helm-swoop-line-number-face`
;;          (setq helm-swoop-use-line-number-face t)))

;  (use-package no-littering :ensure t )

;;      (use-package org-ac
;;        :ensure
;;        :init
;;        (progn
;;          (require 'org-ac)
;;          ;; Make config suit for you. About the config item, eval the following sexp.
;;          ;; (customize-group "org-ac")
;;          (org-ac/config-default)
;;          ) )

;(use-package paradox :ensure t :defer t)

;;(use-package tex :ensure auctex :defer t  :config (require 'tex) )

;;(add-hook 'LaTeX-mode-hook (lambda ()
;;  (push 
;;    '("Latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
;;      :help "Run Latexmk on file")
;;    TeX-command-list)))

;; (use-package vimish-fold
;;   :ensure t
;;   :config
;;   (progn
;;     (evil-leader/set-key "ff" 'vimish-fold)
;;     (evil-leader/set-key "fd" 'vimish-fold-delete)
;;     ))

;; (use-package yasnippet
;;   :ensure t
;;   :diminish ""
;;   :config
;;   (progn
;;   (yas-global-mode 1)
;;     (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
;;     ))

;; (use-package common-lisp-snippets
;;   :ensure t
;;   :config (require 'common-lisp-snippets))

;; (define-key org-mode-map (kbd "M-e") nil)
;; (define-key org-mode-map (kbd "M-a") nil)
;; (define-key org-mode-map (kbd "C-'") nil)
;; (define-key org-mode-map (kbd "M-{") nil)
;; (define-key org-mode-map (kbd "M-}") nil)
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-motion-state-map (kbd "C-y") nil)
(define-key evil-motion-state-map (kbd "C-d") nil)

;; (global-set-key (kbd "C-M-F") 'multi-occur)  
;; (global-set-key (kbd "C-M-[") 'winner-undo)
;; (global-set-key (kbd "C-M-]") 'winner-redo)
;; (global-set-key (kbd "C-M-e") 'open-in-webstorm )
;; (global-set-key (kbd "C-M-f") 'projectile-ag)  ;;search in file
;; (global-set-key (kbd "C-M-g") 'magit-status)
;; (global-set-key (kbd "C-M-n") 'evil-buffer-new)
;; (global-set-key (kbd "C-M-o") 'projectile-switch-project)
;; (global-set-key (kbd "C-M-p") 'package-install)
;; (global-set-key (kbd "C-M-q") 'save-buffers-kill-emacs)
;; (global-set-key (kbd "C-M-z") 'git-gutter:revert-hunk)
;; (global-set-key (kbd "H-n") 'evil-force-normal-state)
;; (global-set-key (kbd "M-?") 'which-key-show-major-mode)
;; (global-set-key (kbd "M-B") 'bookmark-set)
;; (global-set-key (kbd "M-D") 'dired-jump-other-window)
;; (global-set-key (kbd "M-F") 'counsel-ag)  ;; search in project/directory
;; (global-set-key (kbd "M-N") 'git-gutter:next-hunk)
;; (global-set-key (kbd "M-O") 'counsel-find-file)  ;;open file
;; (global-set-key (kbd "M-P") 'git-gutter:previous-hunk)
;; (global-set-key (kbd "M-W") 'kill-other-buffer)
;; (global-set-key (kbd "M-Z")  'undo-tree-redo)
;; (global-set-key (kbd "M-[") 'previous-buffer)
;; (global-set-key (kbd "M-]") 'next-buffer)
;; (global-set-key (kbd "M-b") 'bookmark-bmenu-list) ;;buffers
;; (global-set-key (kbd "M-d") 'dired-jump)
;; (global-set-key (kbd "M-g") 'ivy-switch-buffer) ;;buffers "go"
;; (global-set-key (kbd "M-i") 'counsel-info-lookup-symbol) ;;search info
;; (global-set-key (kbd "M-m") 'counsel-mark-ring) ;;mark rings
;; (global-set-key (kbd "M-n") 'next-error)
;; (global-set-key (kbd "M-o") 'other-window)
;; (global-set-key (kbd "M-p") 'previous-error)
;; (global-set-key (kbd "M-r")  'revert-buffer)
;; (global-set-key (kbd "M-s") 'save-all)
;; (global-set-key (kbd "M-w") 'kill-this-buffer)
;; (global-set-key (kbd "M-y") 'counsel-yank-pop) ;;kill ring browse
;; (global-set-key (kbd "M-z")  'undo)	       ;
;; (global-set-key (kbd "M-{") 'backward-global-mark)
;; (global-set-key (kbd "M-}") 'forward-global-mark)
;; (global-set-key (kbd "s-T") 'display-time)
;; (global-set-key (kbd "s-a") 'org-agenda-list)
;; (global-set-key (kbd "s-b") 'ibuffer)
;; (global-set-key (kbd "s-c") 'cfw:open-org-calendar)
;; (global-set-key (kbd "s-f")  'elfeed)
;; (global-set-key (kbd "s-m") 'mu4e) ;;mark rings
;; (global-set-key (kbd "s-p") 'package-list-packages-no-fetch)
;; (global-set-key (kbd "s-q") 'svisual-fill-column-mode)
;(global-set-key (kbd "s-s")  'sort-lines)
;;(global-set-key (kbd "s-t")  '(lambda()(interactive)(term "zsh")))
;(global-set-key (kbd "s-z") 'undo-tree-visualize)

;;emacs style

(global-set-key (kbd "C-c t")  '(lambda()(interactive)(term "zsh")))
(global-set-key (kbd "C-s") 'swiper)  ;;search in file;;swiper?
(global-set-key (kbd "C-x ,") 'edit-init-org-file)
(global-set-key (kbd "C-x <f2>") 'open-in-webstorm)
(global-set-key (kbd "C-x <f5>") 'toggle-dark-light-state)
(global-set-key (kbd "C-x C-,") 'load-init-file)
(global-set-key (kbd "C-x C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'halt)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-r") 'counsel-recentf) ;;recent
(global-set-key (kbd "C-x M-t") 'vertical-horizontal-swizzle)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-0") 'delete-window-balance)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below-focus)
(global-set-key (kbd "M-3") 'split-window-right-focus)
(global-set-key (kbd "M-t") 'swap-buffers)
(global-set-key (kbd "M-x") 'counsel-M-x) ;;M-x

(define-key evil-normal-state-map (kbd "-") 'dired-jump)

;; (define-key evil-normal-state-map (kbd "C-n") 'other-window)
;; (define-key evil-normal-state-map (kbd "C-p") 'prev-window)

(global-set-key (kbd "C-h h") 'not-anymore)
(global-set-key (kbd "C-h C-a") 'not-anymore)

(set-register ?e (cons 'file "~/.emacs.d/emacs-init.org"))
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
;; (use-package soft-morning-theme :ensure t )
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


(use-package soft-charcoal-theme :ensure t )
(use-package hydandata-light-theme :ensure t)

(defun load-dark ()
  (load-theme 'soft-charcoal t)
  (setq dark-light-state :dark ))

(defun load-light ()
  (load-theme 'hydandata-light t)
  (setq dark-light-state :light ))

(defun toggle-dark-light-state ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (if (eq dark-light-state :dark)
      (load-light)
    (load-dark)))

(defun init-themes ()
  (load-light))

(init-themes)

;; (load-file "~/.emacs.d/secrets.el.gpg")

(use-package minions :ensure t :bind ("<S-down-mouse-3>" . #'minions-minor-modes-menu))
