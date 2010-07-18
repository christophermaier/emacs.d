;; Just how I want things for editing Clojure code

;; Let's turn on Paredit by default, shall we?
(add-hook 'clojure-mode-hook
	  (lambda () 
	    (paredit-mode +1)))

;; Paredit would be sweet to have in the REPL, too
;; Strictly speaking, this could probably go somewhere other than a Clojure
;; configuration file, but I currently don't do any REPL work in anything
;; but Clojure, so here it stays
(add-hook 'slime-repl-mode-hook
	  (lambda ()
	    (paredit-mode +1)))

(provide 'clojure-config)
