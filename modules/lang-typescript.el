;;; lang-typescript.el --- TypeScript/JavaScript IDE -*- lexical-binding: t; -*-

;;; Commentary:
;; TypeScript IDE experience with eglot, tree-sitter, eslint, prettier, jest
;;
;; Key bindings:
;; | Key       | Command                | Description                      |
;; |-----------+------------------------+----------------------------------|
;; | M-.       | xref-find-definitions  | Go to definition                 |
;; | M-?       | xref-find-references   | Find all references              |
;; | M-,       | xref-go-back           | Go back after jumping            |
;; | C-c C-r   | eglot-rename           | Rename symbol across project     |
;; | C-c C-a   | eglot-code-actions     | Show available code actions      |
;; | C-c C-f   | eglot-format           | Format buffer                    |
;; | C-h .     | eldoc                  | Show type/docs at point          |
;; | C-c o     | combobulate prefix     | Structural editing commands      |
;; | C-c C-t   | jest-test-mode prefix  | Run tests                        |

;;; Code:

;; Eglot (LSP) - built-in to Emacs 29+
;; Requires: npm install -g typescript-language-server typescript
(use-package eglot
  :ensure nil
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-confirm-server-initiated-edits nil)
  ;; Performance tuning
  (setq eglot-events-buffer-size 0)
  (fset #'jsonrpc--log-event #'ignore)
  ;; Add typescript-language-server
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode js-ts-mode)
                 . ("typescript-language-server" "--stdio"))))

;; TypeScript tree-sitter mode - built-in to Emacs 29+
(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :custom
  (typescript-ts-mode-indent-offset 2))

;; Flymake-eslint (uses npx to find project-local eslint)
(use-package flymake-eslint
  :ensure t
  :config
  (setq flymake-eslint-executable-name "npx")
  (setq flymake-eslint-executable-args '("eslint"))
  :hook ((typescript-ts-mode . flymake-eslint-enable)
         (tsx-ts-mode . flymake-eslint-enable)
         (js-ts-mode . flymake-eslint-enable)))

;; Prettier formatting
;; Requires: npm install -g prettier
(use-package prettier
  :ensure t
  :hook ((typescript-ts-mode . prettier-mode)
         (tsx-ts-mode . prettier-mode)
         (js-ts-mode . prettier-mode)
         (json-ts-mode . prettier-mode)
         (css-ts-mode . prettier-mode)))

;; Jest test runner
(use-package jest-test-mode
  :ensure t
  :commands jest-test-mode
  :hook ((typescript-ts-mode . jest-test-mode)
         (tsx-ts-mode . jest-test-mode)
         (js-ts-mode . jest-test-mode))
  :config
  (setq jest-test-command-string "npx jest %s"))

;; Combobulate - structural editing for tree-sitter
(use-package combobulate
  :straight (:host github :repo "mickeynp/combobulate")
  :preface
  (setq combobulate-key-prefix "C-c o")
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))

;; Add-node-modules-path - use project-local binaries
(use-package add-node-modules-path
  :ensure t
  :hook ((typescript-ts-mode . add-node-modules-path)
         (tsx-ts-mode . add-node-modules-path)
         (js-ts-mode . add-node-modules-path)))

(provide 'lang-typescript)
;;; lang-typescript.el ends here
