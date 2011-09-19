;; Random configuration tidbits go here
;;
;; See http://github.com/bbatsov/emacs.d/raw/master/misc-config.el for other
;; nice tidbits

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Swiped from Steve Purcell
;; (`https://github.com/purcell/emacs.d/blob/master/init-exec-path.el') (with mods by me)
;;
;; Apparently we need to use `--login' (instead of `-i') to also capture any PATH entries that OS X brings in
;; via it's `/usr/libexec/path_helper' utility, which is only invoked for login shells.
;; This will incorporate any settings from `~/.bash_profile' as well, so I just source `~/.bashrc' from there
;; to get everything at once.
(defun set-exec-path-from-shell-PATH ()
 (let ((path-from-shell (shell-command-to-string "$SHELL --login -c 'echo $PATH'")))
   (setenv "PATH" path-from-shell)
   (setq exec-path (split-string path-from-shell path-separator))))
(if window-system (set-exec-path-from-shell-PATH))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))


;; highlight the current line; set a custom face, so we can
;; recognize from the normal marking (selection)
(defface hi-line '((t (:background "#e8f2fe")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
(global-hl-line-mode t) ; turn it on for all modes by default


(require 'bar-cursor)
(bar-cursor-mode 1)


(provide 'miscellaneous-config)
