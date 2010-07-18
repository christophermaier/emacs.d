
(add-hook 'clojure-mode-hook
	  (lambda () 
	    (paredit-mode +1)))

(provide 'clojure-config)
