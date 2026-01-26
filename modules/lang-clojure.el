;;; lang-clojure.el --- Clojure IDE configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Clojure IDE experience with CIDER, clj-refactor, paredit
;;
;; Requirements:
;; - Clojure CLI: brew install clojure/tools/clojure (macOS)
;;   or Leiningen: brew install leiningen
;; - For ClojureScript: npm install -g shadow-cljs (optional)
;;
;; Getting started:
;; - Open a .clj file and run C-c M-j (cider-jack-in) to start a REPL
;; - For deps.edn projects, jack-in will use clj
;; - For project.clj, jack-in will use lein
;;
;; Key bindings (CIDER):
;; | Key       | Command                     | Description                    |
;; |-----------+-----------------------------+--------------------------------|
;; | C-c C-c   | cider-eval-defun-at-point   | Eval current form              |
;; | C-c C-e   | cider-eval-last-sexp        | Eval sexp before point         |
;; | C-c C-k   | cider-load-buffer           | Load current buffer            |
;; | C-c C-z   | cider-switch-to-repl-buffer | Switch to REPL                 |
;; | C-c M-j   | cider-jack-in               | Start REPL and connect         |
;; | C-c M-J   | cider-jack-in-cljs          | Start ClojureScript REPL       |
;; | M-.       | cider-find-var              | Go to definition               |
;; | M-,       | cider-pop-back              | Go back                        |
;; | C-c C-d d | cider-doc                   | Show documentation             |
;; | C-c C-d j | cider-javadoc               | Show Javadoc                   |
;; | C-c C-m   | clj-refactor prefix         | Refactoring commands           |

;;; Code:

;; Clojure mode
(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.boot\\'" . clojure-mode))
  :hook ((clojure-mode . eldoc-mode)
         (clojure-mode . subword-mode))
  :config
  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-forms-automatically t))

;; CIDER - Clojure IDE
(use-package cider
  :ensure t
  :hook ((clojure-mode . cider-mode)
         (cider-repl-mode . eldoc-mode))
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-history-file "~/.emacs.d/var/cider-repl-history")
  (setq cider-repl-wrap-history t)
  (setq cider-save-file-on-load t)
  ;; Use enrich-classpath for better java source navigation
  (setq cider-enrich-classpath t)
  ;; Pretty print results
  (setq cider-repl-use-pretty-printing t))

;; Clojure refactoring tools
(use-package clj-refactor
  :ensure t
  :diminish clj-refactor-mode
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;; Paredit for Clojure
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :hook ((clojure-mode . enable-paredit-mode)
         (cider-repl-mode . enable-paredit-mode)))

(provide 'lang-clojure)
;;; lang-clojure.el ends here
