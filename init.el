;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; This file loads modular configuration from ~/.emacs.d/modules/
;;
;; Module loading order matters - core must come first to set up
;; straight.el and use-package.

;;; Code:

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Measure startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Core - must load first (straight.el, use-package, basic settings)
(require 'core)

;; macOS specific (load early for PATH setup)
(require 'macos)

;; UI (themes, visual settings)
(require 'ui)

;; Completion framework
(require 'completion)

;; Editing enhancements
(require 'editing)

;; Evil mode
(require 'evil)

;; Org-mode
(require 'org)

;; Git tools
(require 'git)

;; Development tools
(require 'tools)

;; Language support
(require 'lang-typescript)
(require 'lang-lisp)
(require 'lang-python)
(require 'lang-misc)

;; Custom functions
(require 'functions)

;; Keybindings (load last to ensure all commands are defined)
(require 'keybindings)

;;; init.el ends here
