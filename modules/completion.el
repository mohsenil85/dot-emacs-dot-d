;;; completion.el --- Completion framework -*- lexical-binding: t; -*-

;;; Commentary:
;; Vertico, Marginalia, Orderless, Corfu, Company

;;; Code:

;; Vertico - vertical completion UI
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t))

;; Marginalia - rich annotations in minibuffer
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Orderless - flexible completion matching
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

;; Consult - enhanced commands with Vertico
(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x r b" . consult-bookmark)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s l" . consult-line)
         ("M-s r" . consult-ripgrep))
  :config
  (setq consult-narrow-key "<"))

;; Corfu - popup completion at point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Corfu popup info
(use-package corfu-popupinfo
  :after corfu
  :straight nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Company (backup/alternative completion)
(use-package company
  :ensure t
  :diminish)

;; Smex - M-x enhancement
(use-package smex :ensure t :defer t)

;; Swiper - isearch replacement
(use-package swiper :ensure t)

(provide 'completion)
;;; completion.el ends here
