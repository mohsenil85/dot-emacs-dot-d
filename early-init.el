;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Runs before init.el and before the GUI is initialized.
;; Used to prevent UI flicker and speed up startup.

;;; Code:

;; Disable package.el (we use straight.el)
(setq package-enable-at-startup nil)

;; Custom file location
(setq custom-file "~/.emacs.d/var/custom")

;; Faster startup: increase GC threshold during init
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Reset GC after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  gc-cons-percentage 0.1)))

;; Suppress warnings during compilation
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence startup message
(setq inhibit-startup-echo-area-message (user-login-name))
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

;; Prevent UI elements from flashing before theme loads
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; Prevent resize flickering
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

;; Don't compact font caches during GC
(setq inhibit-compacting-font-caches t)

;; Frame configuration: size, position, font
(setq default-frame-alist
      '((font . "Monaco 16")
        (height . 42)
        (width . 144)
        (top . 0)
        (left . 0)))

;;; early-init.el ends here
