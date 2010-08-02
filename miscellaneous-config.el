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


(require 'bar-cursor)
(bar-cursor-mode 1)


(provide 'miscellaneous-config)
