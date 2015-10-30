(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list
   (quote
    ("/usr/local/share/info" "/usr/local/Cellar/emacs-mac/emacs-24.5-z-mac-5.11/share/info/emacs
")))
 '(custom-safe-themes
   (quote
    ("4af6fad34321a1ce23d8ab3486c662de122e8c6c1de97baed3aa4c10fe55e060" "ac2b1fed9c0f0190045359327e963ddad250e131fbf332e80d371b2e1dbc1dc4" "a33ccce31f398f812561d4b0a9f7a2b7038197b722abc278f50d80f3566a0fdf" default)))
 '(eclim-executable
   (quote
    ("/opt/homebrew-cask/Caskroom/eclipse-jee/4.5/Eclipse.app/Contents/Eclipse/eclim")))
 '(elfeed-feeds
   (quote
    ("http://www.newyorker.com/feed/humor" "http://www.newyorker.com/feed/articles" "http://feeds.feedburner.com/TheAtlantic?format=xml" "http://www.lunarbaboon.com/comics/rss.xml" "http://www.atlasobscura.com/feeds/places" "http://pragmaticemacs.com/feed/" "http://www.skyandtelescope.com/astronomy-news/observing-news/feed/" "http://planet.lisp.org/rss20.xml" "http://lisptips.com/rss" "http://what-if.xkcd.com/feed.atom" "http://xkcd.com/rss.xml" "http://www.smbc-comics.com/rss.php" "http://www.qwantz.com/rssfeed.php" "http://languagelog.ldc.upenn.edu/nll/?feed=rss2" "http://www.npr.org/rss/rss.php?id=2" "http://www.npr.org/rss/rss.php?id=1004")))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp track)))
 '(erc-nick "lmohseni")
 '(exec-path
   (quote
    ("/Users/lmohseni/.rvm/gems/ruby-2.1.2/bin" "/Users/lmohseni/.rvm/gems/ruby-2.1.2@global/bin" "/Users/lmohseni/.rvm/rubies/ruby-2.1.2/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/opt/X11/bin" "/usr/texbin" "/Users/lmohseni/.rvm/bin" "/opt/homebrew-cask/Caskroom/emacs-mac/emacs-24.5-z-mac-5.11/Emacs.app/Contents/MacOS/libexec" "/opt/homebrew-cask/Caskroom/emacs-mac/emacs-24.5-z-mac-5.11/Emacs.app/Contents/MacOS/bin" "/Users/lmohseni/.bin")))
 '(git-gutter:window-width -1)
 '(global-git-gutter-mode t)
 '(global-highlight-changes-mode nil)
 '(global-hl-line-mode t)
 '(global-page-break-lines-mode t)
 '(helm-locate-command "mdfind %s %s")
 '(org-agenda-custom-commands
   (quote
    (("x" "daily " tags "inbox"
      ((org-agenda-overriding-header "inbox")
       (org-agenda-include-diary t)
       (org-deadline-warning-days 10)))
     ("n" "Agenda and all TODO's"
      ((agenda "" nil)
       (alltodo "" nil))
      nil))) t)
 '(org-agenda-files (quote ("~/org/work.org" "~/org/organizer.org")))
 '(projectile-global-mode t)
 '(projectile-mode-line (quote (:eval (format " %s" (projectile-project-name)))))
 '(projectile-switch-project-action (quote helm-projectile-find-file)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added ((t (:foreground "medium sea green" :weight bold))))
 '(git-gutter:deleted ((t (:foreground "tomato" :weight bold))))
 '(git-gutter:modified ((t (:foreground "violet red" :weight bold))))
 '(git-gutter:separator ((t (:foreground "deep sky blue" :weight bold))))
 '(mode-line-inactive ((t (:background "gray50" :foreground "gray83"))))
 '(powerline-active2 ((t (:background "grey40" :foreground "PaleGreen1")))))
(require 'ob-tangle)
;;(setq debug-on-error t)
(org-babel-load-file
 (expand-file-name "emacs-init.org"
                   user-emacs-directory))
(put 'narrow-to-page 'disabled nil)
