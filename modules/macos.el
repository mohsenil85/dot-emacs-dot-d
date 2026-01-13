;;; macos.el --- macOS specific configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; macOS-specific settings, modifiers, clipboard, PATH

;;; Code:

(when (eq system-type 'darwin)
  ;; Modifier keys
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mac-control-modifier 'control)
  (setq mac-function-modifier 'hyper)
  (setq mac-pass-command-to-system nil)

  ;; Fullscreen toggle
  (defun toggle-fullscreen ()
    "Toggle full screen."
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

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

(provide 'macos)
;;; macos.el ends here
