;;; lang-rust.el --- Rust IDE configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Rust IDE experience with eglot (rust-analyzer), tree-sitter, cargo
;;
;; Requirements:
;; - rustup (https://rustup.rs)
;; - rust-analyzer: rustup component add rust-analyzer
;; - tree-sitter grammar: M-x treesit-install-language-grammar RET rust
;;
;; Key bindings:
;; | Key       | Command                | Description                      |
;; |-----------+------------------------+----------------------------------|
;; | M-.       | xref-find-definitions  | Go to definition                 |
;; | M-?       | xref-find-references   | Find all references              |
;; | M-,       | xref-go-back           | Go back after jumping            |
;; | C-c C-r   | eglot-rename           | Rename symbol across project     |
;; | C-c C-a   | eglot-code-actions     | Show available code actions      |
;; | C-c C-f   | eglot-format           | Format buffer (rustfmt)          |
;; | C-c C-c   | rust-compile           | Compile project                  |
;; | C-c C-k   | rust-check             | Check project                    |
;; | C-c C-t   | rust-test              | Run tests                        |

;;; Code:

;; Add rust to tree-sitter sources
(add-to-list 'treesit-language-source-alist
             '(rust "https://github.com/tree-sitter/tree-sitter-rust"))

;; Rust tree-sitter mode - built-in to Emacs 29+
(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :hook ((rust-ts-mode . eglot-ensure))
  :config
  ;; Use rust-analyzer through eglot
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer"))))

;; Cargo integration
(use-package cargo
  :ensure t
  :hook (rust-ts-mode . cargo-minor-mode)
  :bind (:map rust-ts-mode-map
              ("C-c C-c" . cargo-process-build)
              ("C-c C-k" . cargo-process-check)
              ("C-c C-t" . cargo-process-test)
              ("C-c C-r" . cargo-process-run)))

;; TOML mode for Cargo.toml
(use-package toml-mode
  :ensure t
  :mode (("Cargo\\.toml\\'" . toml-mode)
         ("\\.toml\\'" . toml-mode)))

(provide 'lang-rust)
;;; lang-rust.el ends here
