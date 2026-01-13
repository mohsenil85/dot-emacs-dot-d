;;; init.el --- Minimal TypeScript config for debugging -*- lexical-binding: t; -*-

;;; Code:

;; Bootstrap straight.el
(setq straight-disable-info t)
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

;; Basic settings
(global-font-lock-mode 1)
(setq treesit-font-lock-level 4)

;; Tree-sitter sources
(setq treesit-language-source-alist
      '((tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))

;; TypeScript mode
(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

;; Eglot (LSP) - built-in to Emacs 29+
;; Requires: npm install -g typescript-language-server typescript
(use-package eglot
  :ensure nil
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c C-r" . eglot-rename)
              ("C-c C-a" . eglot-code-actions)
              ("C-c C-f" . prettier-prettify))
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-confirm-server-initiated-edits nil)
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode js-ts-mode)
                 . ("typescript-language-server" "--stdio"))))

;; Add-node-modules-path - use project-local eslint/prettier
(use-package add-node-modules-path
  :ensure t
  :hook ((typescript-ts-mode . add-node-modules-path)
         (tsx-ts-mode . add-node-modules-path)
         (js-ts-mode . add-node-modules-path)))

;; EditorConfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Prettier
(use-package prettier
  :ensure t
  :hook ((typescript-ts-mode . prettier-mode)
         (tsx-ts-mode . prettier-mode)
         (js-ts-mode . prettier-mode)
         (json-ts-mode . prettier-mode)
         (css-ts-mode . prettier-mode)))

;; Flymake-eslint (uses npx to find project-local eslint)
(use-package flymake-eslint
  :ensure t
  :config
  (setq flymake-eslint-executable-name "npx")
  (setq flymake-eslint-executable-args '("eslint"))
  :hook ((typescript-ts-mode . flymake-eslint-enable)
         (tsx-ts-mode . flymake-eslint-enable)
         (js-ts-mode . flymake-eslint-enable)))

;; Theme so we can see colors
(load-theme 'modus-operandi t)

;; Completion framework
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-s l" . consult-line)
         ("M-s r" . consult-ripgrep)))

;; Dired
(use-package dired
  :straight nil
  :bind (:map dired-mode-map
              (("`" . dired-toggle-read-only)
               ("-" . dired-up-directory)
               ("~" . (lambda () (interactive) (find-alternate-file "~/")))
               ("RET" . dired-find-file)
               ("C-<return>" . dired-find-file-other-window))))

(use-package dired+
  :straight (dired+ :fetcher url :url "https://www.emacswiki.org/emacs/download/dired+.el")
  :defer 1
  :init
  (setq diredp-hide-details-initially-flag t)
  (setq diredp-hide-details-propagate-flag t)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

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

;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-want-unimpaired-p nil))

(use-package evil-visual-mark-mode
  :ensure t
  :config
  (evil-visual-mark-mode 1))

;; ESC quits things
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key evil-normal-state-map (kbd "M-y") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-motion-state-map (kbd "C-y") nil)
  (define-key evil-motion-state-map (kbd "C-d") nil))

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)
(global-set-key (kbd "C-z") 'evil-mode)

;; Recentf
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)

;; macOS specific
(when (eq system-type 'darwin)
  ;; Modifier keys
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mac-control-modifier 'control)
  (setq mac-function-modifier 'hyper)
  (setq mac-pass-command-to-system nil)

  ;; Clipboard functions
  (defun pbcopy ()
    "Copy region to macOS clipboard."
    (interactive)
    (call-process-region (point) (mark) "pbcopy")
    (setq deactivate-mark t))

  (defun pbpaste ()
    "Paste from macOS clipboard."
    (interactive)
    (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

  (defun pbcut ()
    "Cut region to macOS clipboard."
    (interactive)
    (pbcopy)
    (delete-region (region-beginning) (region-end)))

  (global-set-key (kbd "C-x M-c") 'pbcopy)
  (global-set-key (kbd "C-x M-v") 'pbpaste)
  (global-set-key (kbd "C-x M-x") 'pbcut)

  ;; Add Homebrew site-lisp to load path
  (let ((default-directory "/opt/homebrew/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))

  ;; Get PATH from shell
  (use-package exec-path-from-shell
    :ensure t
    :init
    (exec-path-from-shell-initialize)))

;;; init.el ends here
