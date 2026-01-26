;;; ui.el --- UI configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Themes, visual tweaks, and UI packages

;;; Code:

;; Modus themes (by Protesilaos)
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t))

(defvar dark-light-state :light
  "Current theme state, :dark or :light.")

(defun load-dark ()
  "Load dark theme."
  (load-theme 'modus-vivendi t)
  (setq dark-light-state :dark))

(defun load-light ()
  "Load light theme."
  (load-theme 'modus-operandi t)
  (setq dark-light-state :light))

(defun reset-themes ()
  "Disable all enabled themes."
  (mapc #'disable-theme custom-enabled-themes))

(defun toggle-dark-light-state ()
  "Toggle between dark and light themes."
  (interactive)
  (reset-themes)
  (if (eq dark-light-state :dark)
      (load-light)
    (load-dark)))

(defun init-themes ()
  "Initialize themes on startup."
  (reset-themes)
  (load-light))

(init-themes)

;; Which-key - show available keybindings
(use-package which-key
  :ensure t
  :diminish ""
  :config
  (which-key-mode))

;; Golden ratio - auto-resize windows
(use-package golden-ratio
  :ensure t
  :diminish ""
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-exclude-modes '(ediff-mode)))

;; Page break lines - display ^L as horizontal lines
(use-package page-break-lines
  :diminish ""
  :ensure t
  :config
  (global-page-break-lines-mode))

;; Writeroom mode for distraction-free writing
(use-package writeroom-mode
  :ensure t
  :defer t)

;; Fullscreen helper
(defun full-frame-irregardless ()
  "Set frame to fullscreen."
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth))

(full-frame-irregardless)

;; Pulse line on scroll/window switch
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
                   recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

;; Image+ with hydra
(use-package image+
  :ensure hydra
  :defer t
  :config
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
         ("l" imagex-sticky-rotate-left "rotate left")))))

(provide 'ui)
;;; ui.el ends here
