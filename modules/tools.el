;;; tools.el --- Development tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Dired, terminal, search, AI assistants, REST client

;;; Code:

;; Dired configuration
(use-package dired
  :straight nil
  :bind (:map dired-mode-map
              (("`" . dired-toggle-read-only)
               ("-" . dired-up-directory)
               ("~" . (lambda () (interactive) (find-alternate-file "~/")))
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
    (diredp-toggle-find-file-reuse-dir 1)))

(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

(use-package dired-hist
  :straight (:host github :repo "karthink/dired-hist" :files ("*.el"))
  :ensure t
  :bind (:map dired-mode-map
              ("l" . dired-hist-go-back)
              ("r" . dired-hist-go-forward))
  :config
  (dired-hist-mode 1))

;; Terminal - Eat
(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package term)

;; Search tools
(use-package ag
  :ensure t
  :defer t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

(use-package rg
  :ensure t
  :defer t
  :config
  (rg-enable-default-bindings))

(use-package fzf :ensure t :defer t)

;; Project management
(use-package project
  :straight nil
  :config
  (setq project-switch-commands 'project-dired))

(use-package project-x
  :straight (:host github :repo "karthink/project-x" :files ("*.el"))
  :after project
  :config
  (setq project-x-save-interval 600)
  (project-x-mode 1))

;; AI assistants
(use-package gptel
  :ensure t
  :defer t
  :commands (gptel gptel-send gptel-menu)
  :config
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (getenv "ANTHROPIC_API_KEY"))
        gptel-model 'claude-opus-4-20250514
        gptel-default-mode 'org-mode))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :config
  (setq copilot-expansion-delay 0.2)
  (setq copilot-expansion-limit 500)
  (setq copilot-node-executable (executable-find "node"))
  :bind (("C-c M-f" . copilot-complete)
         :map copilot-completion-map
         ("C-g" . copilot-clear-overlay)
         ("M-p" . copilot-previous-completion)
         ("M-n" . copilot-next-completion)
         ("<tab>" . copilot-accept-completion)
         ("M-f" . copilot-accept-completion-by-word)
         ("M-<return>" . copilot-accept-completion-by-line)))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (setq claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-emacs-tools-setup))

;; REST client
(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package ob-restclient
  :ensure t
  :after (restclient org)
  :config
  (setq org-babel-restclient--jq-path "/opt/homebrew/bin/jq")
  (add-to-list 'org-babel-load-languages '(restclient . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; Misc tools
(use-package bind-map :ensure t :defer t)
(use-package emojify :ensure t :defer t)
(use-package feebleline :ensure t :defer t)
(use-package speed-type
  :ensure t
  :custom
  (speed-type-default-lang 'English))

(provide 'tools)
;;; tools.el ends here
