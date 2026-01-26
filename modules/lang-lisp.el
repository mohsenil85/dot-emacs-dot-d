;;; lang-lisp.el --- Lisp languages configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs Lisp and Common Lisp configuration
;; See lang-clojure.el for Clojure configuration
;;
;; Requirements (for Common Lisp):
;; - SBCL: brew install sbcl (macOS)
;;   or another Common Lisp: brew install ccl (Clozure CL)
;;
;; Getting started:
;; - M-x sly to start a Common Lisp REPL
;; - C-c C-c to compile defun at point
;; - C-c C-k to compile and load file

;;; Code:

;; Eldoc for Emacs Lisp
(use-package eldoc
  :diminish
  :hook ((emacs-lisp-mode . eldoc-mode)
         (lisp-interaction-mode . eldoc-mode)
         (lisp-mode . eldoc-mode)))

;; Paredit for structured editing (Emacs Lisp, Common Lisp, Scheme)
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)))

;; Common Lisp with SLY (modern SLIME fork)
(use-package sly
  :ensure t
  :commands sly
  :config
  (setq inferior-lisp-program "sbcl"))

(provide 'lang-lisp)
;;; lang-lisp.el ends here
