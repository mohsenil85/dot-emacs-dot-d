;;; evil.el --- Evil mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Evil mode, evil-collection, evil-org

;;; Code:

;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  ;; Evil is loaded but not enabled by default
  ;; Toggle with C-z
  )

;; Evil collection - bindings for many modes
(use-package evil-collection
  :after evil
  :diminish
  :ensure t
  :config
  (setq evil-collection-want-unimpaired-p nil))

;; Evil visual mark mode
(use-package evil-visual-mark-mode
  :ensure t
  :config
  (evil-visual-mark-mode 1))

;; Evil org
(use-package evil-org
  :ensure t
  :after org
  :diminish
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)

  ;; Sync evil-org-mode with evil-mode toggle
  (defun my/sync-evil-org-mode ()
    "Enable or disable evil-org-mode based on evil-mode state."
    (if (bound-and-true-p evil-mode)
        (evil-org-mode 1)
      (evil-org-mode -1)))

  ;; When opening org buffers, sync evil-org state
  (add-hook 'org-mode-hook #'my/sync-evil-org-mode)

  ;; When toggling evil-mode, update all open org buffers
  (add-hook 'evil-mode-hook
            (lambda ()
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (when (derived-mode-p 'org-mode)
                    (my/sync-evil-org-mode)))))))

;; Big escape - make ESC quit things
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
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

(provide 'evil)
;;; evil.el ends here
