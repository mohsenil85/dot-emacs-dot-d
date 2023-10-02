(setq package-enable-at-startup nil)
(setq custom-file  "~/.emacs.d/var/custom" )


;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(tool-bar-mode -1)                      ; All these tools are in the menu-bar anyway

  ;;bigger font size for my poor old aching occulars
  (setq default-frame-alist  '(
  				      (font . "-*-Monaco-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"   )
  				      (height . 42)
  				      (width . 144)
  				      (top . 0)
  				      (left . 0)
  				      )) 

