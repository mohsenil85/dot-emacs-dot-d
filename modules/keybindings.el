;;; keybindings.el --- Key bindings and registers -*- lexical-binding: t; -*-

;;; Commentary:
;; Global keybindings and file registers

;;; Code:

;; Function keys
(global-set-key (kbd "<f1>") 'eat)
(global-set-key (kbd "<f2>") 'magit-status)
(global-set-key (kbd "<f5>") 'mu4e)
(global-set-key (kbd "<f6>") 'org-agenda)
(global-set-key (kbd "<f7>") 'rg)
(global-set-key (kbd "<f8>") 'compile)
(global-set-key (kbd "<f9>") 'speed-type-top-1000)

;; C-x prefixed
(global-set-key (kbd "C-x M-r") 'revert-this-buffer)
(global-set-key (kbd "C-<f2>") 'multi-occur-in-this-mode)
(global-set-key (kbd "C-h C-/") 'which-key-show-major-mode)
(global-set-key (kbd "C-x ,") 'edit-init-org-file)
(global-set-key (kbd "C-x <f2>") 'open-in-webstorm)
(global-set-key (kbd "C-x <f5>") 'toggle-dark-light-state)
(global-set-key (kbd "C-x C-,") 'load-init-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'halt)
(global-set-key (kbd "C-x d") 'dired-jump)
(global-set-key (kbd "C-x C-r") 'recentf)
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x m") 'mu4e-compose-new)

;; M-prefixed
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-F") 'forward-word)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; C-prefixed
(global-set-key (kbd "C-o") 'smart-open-line)
(global-set-key (kbd "C-S-o") 'smart-open-line-above)
(global-set-key (kbd "C-<tab>") 'hs-cycle)
(global-set-key (kbd "C-S-<tab>") 'hs-cycle)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-z") 'evil-mode)

;; Super/special keys
(global-set-key (kbd "s-SPC") 'cycle-spacing)

;; Window/buffer manipulation
(global-set-key (kbd "C-c M-t") 'swap-buffers)
(global-set-key (kbd "C-x M-t") 'vertical-horizontal-swizzle)

;; Mark navigation
(global-set-key (kbd "C-`") 'jump-to-mark)
(global-set-key (kbd "M-`") 'backward-global-mark)
(global-set-key (kbd "C-M-`") 'forward-global-mark)
(global-set-key (kbd "C-SPC") 'push-mark-no-activate)
(global-set-key (kbd "C-S-SPC") 'set-mark-command)

;; Remap exchange-point-and-mark
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; Evil mode toggle in normal state
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-z") 'evil-mode))

;; Disabled functions
(global-set-key (kbd "C-h h") 'not-anymore)
(global-set-key (kbd "C-h C-a") 'not-anymore)

;; File registers
(set-register ?e (cons 'file "~/.emacs.d/init.el"))
(set-register ?o (cons 'file "~/org/main.org"))
(set-register ?i (cons 'file "~/org/inbox.org"))
(set-register ?n (cons 'file "~/org/notes.org"))
(set-register ?w (cons 'file "~/org/work.org"))
(set-register ?z (cons 'file "~/.zshrc"))
(set-register ?d (cons 'file "~/Projects/lisp/drogue/drogue.lisp"))
(set-register ?p (cons 'file "~/Projects/"))
(set-register ?s (cons 'file "~/org/stories/ideas.org"))
(set-register ?k (cons 'file "~/Projects/Builds/qmk_firmware/keyboards/ergodox_ez/keymaps/mohsenil85/keymap.c"))

(provide 'keybindings)
;;; keybindings.el ends here
