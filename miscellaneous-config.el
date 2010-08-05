;; Random configuration tidbits go here
;;
;; See http://github.com/bbatsov/emacs.d/raw/master/misc-config.el for other
;; nice tidbits

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable startup screen
(setq inhibit-startup-screen t)

;; I want to see line and column numbers
(global-linum-mode 1) 
(setq line-number-mode t)
(setq column-number-mode t)
(setq size-indication-mode t)

;; Sets the Option key to act as a Meta key (on a Mac)
;; ESC still acts as a Meta key, though
;;(setq mac-option-modifier 'meta)

;; Actually, I think I'm going to try using the Command key as Meta instead
;; This is from http://www.emacswiki.org/emacs/MetaKeyProblems
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)


;; End all files with a newline
(setq require-final-newline t)

;; FINALLY... not having this was driving me insane
(global-set-key "\C-a" 'beginning-of-line-text)

;; backups and autosaves go to temp folder
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(fset 'yes-or-no-p 'y-or-n-p)  		; enable y/n answers to yes/no

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

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

(require 'bar-cursor)
(bar-cursor-mode 1)


(provide 'miscellaneous-config)
