;;; core.el --- Core Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Bootstrap straight.el, use-package, and core Emacs settings

;;; Code:

;; Bootstrap straight.el
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

;; Diminish - hide minor modes from modeline
(use-package diminish :ensure t)
(diminish 'subword-mode)
(diminish 'visual-line-mode)
(diminish 'auto-revert-mode)

;; No-littering - keep .emacs.d clean
(use-package no-littering :ensure t)

;; Bind-key
(use-package bind-key
  :demand
  :init
  (setq bind-key-describe-special-forms t)
  :config
  (require 'bind-key))

;; Better defaults (but disable ido - we use vertico)
(use-package better-defaults
  :ensure t
  :config
  (ido-mode -1))

;; Core Emacs settings
(use-package emacs
  :init
  (require 'misc)

  ;; Add prompt indicator to `completing-read-multiple'
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

  ;; UI settings
  (blink-cursor-mode -1)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (delete-selection-mode 1)
  (electric-pair-mode 1)
  (size-indication-mode 1)
  (global-display-line-numbers-mode 1)
  (toggle-word-wrap 1)
  (prefer-coding-system 'utf-8)
  (recentf-mode 1)
  (server-start)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8-unix)
  (tooltip-mode -1)
  (setq tooltip-use-echo-area t)
  (context-menu-mode)
  (pixel-scroll-precision-mode)
  (column-number-mode 1)

  (setq
   view-read-only t
   xref-search-program 'ripgrep
   display-time-default-load-average nil
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   auto-save-visited-interval 1
   auto-save-visited-mode 1
   backup-by-copying t
   backup-directory-alist `((".*" . ,temporary-file-directory))
   confirm-kill-processes nil
   confirm-nonexistent-file-or-buffer nil
   default-fill-column 80
   delete-old-versions t
   explicit-shell-file-name "/bin/zsh"
   explicit-zsh-args '("--login" "--interactive")
   history-length 250
   indicate-empty-lines t
   initial-scratch-message ";         :D"
   kill-ring-max 5000
   load-prefer-newer t
   locale-coding-system 'utf-8
   mark-ring-max 5000
   recentf-max-saved-items 5000
   sentence-end-double-space t
   shell-file-name "/bin/zsh"
   show-paren-delay 0
   show-paren-style 'parenthesis
   show-paren-when-point-inside-paren t
   switch-to-buffer-preserve-window-point t
   tab-always-indent 'complete
   tooltip-use-echo-area t
   use-dialog-box nil
   user-full-name "Logan Mohseni"
   user-mail-address "logan@mohseni.io"
   vc-follow-symlinks t
   vc-make-backup-files t
   version-control t)

  (setq-default indicate-buffer-boundaries 'left)
  (setq display-time-format "%l:%M")
  (setq display-time-interval 1)
  (display-time-mode)

  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

;; Shell setup
(defun zsh-shell-mode-setup ()
  (setq-local comint-process-echoes t))
(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

;; Uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Winner mode for window undo
(winner-mode 1)

;; Colorize compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Paren matching
(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode +1))

;; Savehist
(use-package savehist
  :config
  (setq savehist-file "~/.emacs.d/var/savehist")
  (setq savehist-additional-variables
        '(kill-ring mark-ring global-mark-ring search-ring
          regexp-search-ring extended-command-history))
  (savehist-mode 1))

;; Saveplace
(use-package saveplace
  :config
  (setq-default save-place t))

;; Persistent scratch
(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))

;; Keyfreq - track command usage
(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(pixel-scroll-precision mwheel-scroll self-insert-command)))

;; Tree-sitter configuration
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

;; Remap modes to tree-sitter versions
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)))

;; Flycheck
(use-package flycheck
  :ensure t
  :diminish "")

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message) line-end))
  :modes (text-mode markdown-mode gfm-mode))
(add-to-list 'flycheck-checkers 'proselint)

;; Dtrt-indent - auto-detect indentation
(use-package dtrt-indent
  :diminish
  :ensure t
  :init
  (dtrt-indent-global-mode 1))

;; General (for keybinding)
(use-package general
  :ensure t
  :defer 1
  :config
  (setq leader "SPC"))

(provide 'core)
;;; core.el ends here
