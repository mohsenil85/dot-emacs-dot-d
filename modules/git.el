;;; git.el --- Git configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Magit, Forge, git-gutter, git-link

;;; Code:

;; Magit
(use-package magit
  :ensure t
  :defer t
  :config
  (define-key transient-map (kbd "<escape>") 'transient-quit-one)
  (setq magit-save-repository-buffers 'dontask))

;; Forge - GitHub/GitLab integration
(use-package forge
  :after magit
  :init
  (setq forge-add-default-bindings t)
  :config
  (setq auth-sources '("~/.authinfo")))

;; Git-gutter - show diff in fringe
(use-package git-gutter
  :ensure t
  :diminish
  :config
  (global-git-gutter-mode t)
  (setq git-gutter:update-interval 0.2)

  (global-set-key (kbd "C-x C-g") 'git-gutter)
  (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
  (global-set-key (kbd "C-x C-p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x C-n") 'git-gutter:next-hunk)
  (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
  (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
  (global-set-key (kbd "C-x v SPC") #'git-gutter:mark-hunk))

;; Git-link - get GitHub URLs
(use-package git-link
  :ensure t
  :diminish)

(provide 'git)
;;; git.el ends here
