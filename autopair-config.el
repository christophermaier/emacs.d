(require 'autopair)
;; Safe to use this, since it defers to paredit-mode when that is enabled
;; See http://www.emacswiki.org/emacs/AutoPairs#toc4
(autopair-global-mode)

;; Autopair doesn't play nicely with SLIME's debugger
;; See http://code.google.com/p/autopair/issues/detail?id=32
(add-hook 'sldb-mode-hook #'(lambda () 
                              (setq autopair-dont-activate t)))

;; Apparently it doesn't like graphviz-dot-mode either
(add-hook 'graphviz-dot-mode-hook #'(lambda () 
                                      (setq autopair-dont-activate t)))
(provide 'autopair-config)
