;;; editing.el --- Editing enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; Paredit, expand-region, avy, visible-mark, smartscan

;;; Code:

;; Paredit - structural editing for Lisp
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode))
  :bind (:map paredit-mode-map
              ("C-j" . paredit-newline)))

;; Expand region - smart region expansion
(use-package expand-region :ensure t)

;; Avy - jump to visible text
(use-package avy
  :ensure t
  :demand t
  :bind (("M-j" . avy-goto-char-timer)))

;; Visible mark - show marks visually
(use-package visible-mark
  :ensure t
  :config
  (global-visible-mark-mode 1))

;; Smartscan - jump to next/prev symbol
(use-package smartscan
  :ensure t
  :demand t
  :hook (prog-mode . smartscan-mode))

;; Origami - folding
(use-package origami :ensure t :defer t)

;; Hide-show cycle functions
(defun hs-cycle (&optional level)
  "Cycle hide/show for current block."
  (interactive "p")
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
               (hs-hide-block)
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

(defun hs-global-cycle ()
  "Cycle hide/show for entire buffer."
  (interactive)
  (pcase last-command
    ('hs-global-cycle
     (save-excursion (hs-show-all))
     (setq this-command 'hs-global-show))
    (_ (hs-hide-all))))

(provide 'editing)
;;; editing.el ends here
