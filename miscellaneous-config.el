;; Random configuration tidbits go here
;;
;; See http://github.com/bbatsov/emacs.d/raw/master/misc-config.el for other
;; nice tidbits

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable startup screen
(setq inhibit-startup-screen t)

;; I want to see line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Sets the Option key to act as a Meta key (on a Mac)
;; ESC still acts as a Meta key, though
(setq mac-option-modifier 'meta)

;; End all files with a newline
(setq require-final-newline t)

;; FINALLY... not having this was driving me insane
(global-set-key "\C-a" 'beginning-of-line-text)




(provide 'miscellaneous-config)
