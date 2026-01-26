;;; lang-java.el --- Java IDE configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Java IDE experience with eglot (jdtls), tree-sitter
;;
;; Requirements:
;; - JDK 17+ (for jdtls)
;; - jdtls: brew install jdtls (macOS) or download from Eclipse
;; - tree-sitter grammar: M-x treesit-install-language-grammar RET java
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

;;; Code:

;; Add java to tree-sitter sources
(add-to-list 'treesit-language-source-alist
             '(java "https://github.com/tree-sitter/tree-sitter-java"))

;; Java tree-sitter mode - built-in to Emacs 29+
(use-package java-ts-mode
  :ensure nil
  :mode "\\.java\\'"
  :hook ((java-ts-mode . eglot-ensure))
  :config
  ;; Configure jdtls for eglot
  ;; jdtls needs a workspace directory per project
  (defun my/java-eglot-init-options ()
    "Return jdtls initialization options."
    `(:settings
      (:java
       (:format (:enabled t)
        :saveActions (:organizeImports t)
        :completion (:enabled t
                     :importOrder ["java" "javax" "org" "com"])))))

  (add-to-list 'eglot-server-programs
               `(java-ts-mode . ("jdtls"
                                 :initializationOptions
                                 ,(my/java-eglot-init-options)))))

;; Gradle/Maven build support via compile
(defun java-compile ()
  "Compile Java project, detecting build tool."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (cond
     ((file-exists-p "gradlew")
      (compile "./gradlew build"))
     ((file-exists-p "build.gradle")
      (compile "gradle build"))
     ((file-exists-p "pom.xml")
      (compile "mvn compile"))
     (t
      (compile "javac *.java")))))

(defun java-test ()
  "Run Java tests, detecting build tool."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (cond
     ((file-exists-p "gradlew")
      (compile "./gradlew test"))
     ((file-exists-p "build.gradle")
      (compile "gradle test"))
     ((file-exists-p "pom.xml")
      (compile "mvn test"))
     (t
      (message "No build tool found")))))

(with-eval-after-load 'java-ts-mode
  (define-key java-ts-mode-map (kbd "C-c C-c") #'java-compile)
  (define-key java-ts-mode-map (kbd "C-c C-t") #'java-test))

(provide 'lang-java)
;;; lang-java.el ends here
