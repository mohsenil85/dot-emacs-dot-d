;;; lang-misc.el --- Miscellaneous language modes -*- lexical-binding: t; -*-

;;; Commentary:
;; HCL, YAML, Markdown, and other language modes

;;; Code:

;; HCL mode (Terraform)
(use-package hcl-mode
  :ensure t
  :mode (("\\.tf\\'" . hcl-mode)
         ("\\.tfvars\\'" . hcl-mode)))

;; Highlight indent guides (for YAML etc)
(use-package highlight-indent-guides
  :ensure t
  :diminish
  :hook (yaml-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; YAML mode
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :defer t)

(provide 'lang-misc)
;;; lang-misc.el ends here
