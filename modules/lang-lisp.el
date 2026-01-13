;;; lang-lisp.el --- Lisp languages configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs Lisp, Clojure, CIDER

;;; Code:

;; Eldoc for Emacs Lisp
(use-package eldoc
  :diminish
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode))

;; Clojure mode
(use-package clojure-mode
  :ensure t
  :defer t)

;; CIDER - Clojure IDE
(use-package cider
  :ensure t
  :defer t)

(add-hook 'clojure-mode-hook #'eldoc-mode)
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

(provide 'lang-lisp)
;;; lang-lisp.el ends here
