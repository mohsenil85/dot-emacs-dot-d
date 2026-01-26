;;; init.el --- Emacs configuration loader -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads modular configuration from ~/.emacs.d/modules/
;; See early-init.el for settings that run before this file.

;;; Code:

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Core: bootstrap straight.el, use-package, and essential settings
(require 'core)

;; Completion: vertico, consult, orderless, corfu
(require 'completion)

;; Editing: undo-tree, expand-region, multiple-cursors
(require 'editing)

;; UI: theme, modeline, helpful
(require 'ui)

;; Evil: vim emulation (disabled by default, toggle with C-z)
(require 'evil)

;; Git: magit and related tools
(require 'git)

;; Languages
(require 'lang-lisp)
(require 'lang-clojure)
(require 'lang-python)
(require 'lang-typescript)
(require 'lang-rust)
(require 'lang-java)
(require 'lang-misc)

;; Tools: project, dired, gptel, etc.
(require 'tools)

;; Org-mode
(require 'org)

;; Custom functions
(require 'functions)

;; Keybindings
(require 'keybindings)

;; macOS specific settings
(when (eq system-type 'darwin)
  (require 'macos))

;;; init.el ends here
