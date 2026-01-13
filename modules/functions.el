;;; functions.el --- Custom functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom utility functions

;;; Code:

(defun copy-filename-to-clip ()
  "Put the current file name on the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun load-init-file ()
  "Reload init.el."
  (interactive)
  (message "loading init...")
  (load-file (concat "~/.emacs.d/init.el")))

(defun edit-init-org-file ()
  "Toggle between init.org and init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun add-hook-to-modes (modes hook)
  "Add HOOK to all MODES."
  (dolist (mode modes)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
              hook)))

(defun halt ()
  "Save all and kill Emacs."
  (interactive)
  (save-some-buffers t)
  (kill-emacs))

(defun my-whitespace-mode-hook ()
  "Setup whitespace mode."
  (setq whitespace-action '(auto-cleanup)
        whitespace-style '(face tabs trailing lines-tail empty)
        whitespace-line-column nil)
  (whitespace-mode))

(defun my-makefile-mode-hook ()
  "Setup makefile mode."
  (setq indent-tabs-mode t
        tab-width 4))

(defun make-region-read-only (start end)
  "Make region from START to END read-only."
  (interactive "*r")
  (let ((inhibit-read-only t))
    (put-text-property start end 'read-only t)))

(defun make-region-read-write (start end)
  "Make region from START to END writable."
  (interactive "*r")
  (let ((inhibit-read-only t))
    (put-text-property start end 'read-only nil)))

(defun swap-buffers ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer))
  (other-window 1))

(defun prev-window ()
  "Select previous window."
  (interactive)
  (other-window -1))

(defun kill-other-buffer ()
  "Kill the buffer in the other window."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window 1))

;; Mark ring navigation
(defun marker-is-point-p (marker)
  "Test if MARKER is current point."
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun push-mark-maybe ()
  "Push mark onto `global-mark-ring' if not at current location."
  (if (not global-mark-ring)
      (error "global-mark-ring empty")
    (unless (or (marker-is-point-p (car global-mark-ring))
                (marker-is-point-p (car (reverse global-mark-ring))))
      (push-mark))))

(defun backward-global-mark ()
  "Use `pop-global-mark', pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark))

(defun forward-global-mark ()
  "Hack `pop-global-mark' to go in reverse."
  (interactive)
  (push-mark-maybe)
  (setq global-mark-ring (nreverse global-mark-ring))
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark)
  (setq global-mark-ring (nreverse global-mark-ring)))

(defun save-all ()
  "Save all file-visiting buffers without prompting."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

;; Window manipulation
(defun delete-window-balance ()
  "Delete window and rebalance the remaining ones."
  (interactive)
  (delete-window)
  (balance-windows))

(defun split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun vertical-horizontal-swizzle ()
  "Toggle between vertical and horizontal split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun not-anymore ()
  "Placeholder for disabled commands."
  (interactive)
  (message "not anymore"))

(defun prose-mode ()
  "Enable distraction-free writing mode."
  (interactive)
  (display-line-numbers-mode 0)
  (writeroom-mode 1)
  (page-break-lines-mode 1)
  (flyspell-mode 1)
  (electric-quote-mode 1)
  (abbrev-mode 1)
  (toggle-word-wrap 1)
  (setq buffer-face-mode-face
        '(:family "Times New Roman"
          :height 180
          :width semi-condensed))
  (buffer-face-mode))

(defun chuck-into-next-weekish ()
  "Sloppily reschedule current org item into next week."
  (interactive)
  (org-schedule nil (format "+%dd" (+ 11 (random 9))))
  (org-set-tags ":chucked:"))

(defun open-in-webstorm ()
  "Open current file in WebStorm."
  (interactive)
  (shell-command (format "webstorm --line %s --column %s %s"
                         (line-number-at-pos)
                         (current-column)
                         (buffer-file-name))))

(defun get-buffers-matching-mode (mode)
  "Return a list of buffers where major-mode equals MODE."
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq mode major-mode)
          (push buf buffer-mode-matches))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defun revert-this-buffer ()
  "Revert current buffer without confirmation."
  (interactive)
  (revert-buffer nil t t)
  (message "reverted %s" (buffer-name)))

(defun smart-open-line ()
  "Insert an empty line after the current line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun push-mark-no-activate ()
  "Push point to mark-ring without activating region."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jump to the local mark, respecting the mark-ring order."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate (arg)
  "Exchange point and mark without activating the region.
With prefix ARG, activate region."
  (interactive "P")
  (exchange-point-and-mark)
  (unless arg (deactivate-mark nil)))

(defun my-clipboard-to-elfeed ()
  "Add clipboard URL to elfeed."
  (interactive)
  (let ((link (pbpaste)))
    (elfeed-add-feed link)))

(provide 'functions)
;;; functions.el ends here
