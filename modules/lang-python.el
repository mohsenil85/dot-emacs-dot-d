;;; lang-python.el --- Python configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Python with Jedi, ob-ipython

;;; Code:

;; Jedi - Python completion
(use-package jedi
  :ensure t
  :defer t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

;; Ob-ipython for org-babel
(use-package ob-ipython
  :ensure t
  :defer t)

(provide 'lang-python)
;;; lang-python.el ends here
