;;; lang-python.el --- Python configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Python with eglot (LSP) and tree-sitter
;; Requires: pip install python-lsp-server (or pyright)

;;; Code:

;; Python tree-sitter mode - built-in to Emacs 29+
(use-package python
  :ensure nil
  :hook ((python-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :config
  (setq python-indent-guess-indent-offset-verbose nil))

(provide 'lang-python)
;;; lang-python.el ends here
