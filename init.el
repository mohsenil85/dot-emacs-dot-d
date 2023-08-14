(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


(load-file "~/.emacs.d/org-init.el")

(use-package no-littering :ensure t)

;;used in debugging
;;(use-package evil :ensure t :config (evil-mode 1))
;;(setq debug-on-error t)

;;(use-package org :ensure t)	

;;(straight-use-package 'org)
(require 'ob-tangle)


(org-babel-load-file
 (expand-file-name "emacs-init.org"
		   user-emacs-directory))
