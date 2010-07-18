
;; Nice to have Paredit around when editing these Emacs files
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (paredit-mode +1)))

(provide 'emacs-lisp-config)
