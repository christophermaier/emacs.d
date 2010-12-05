(load-file "~/.emacs.d/ext/graphviz-dot-mode/graphviz-dot-mode.el")

;; Still haven't sorted out path issues on Mac... just give full path for now
(setq graphviz-dot-dot-program "/usr/local/bin/dot")

;; I don't have "doted" on my Mac... but I do have Graphviz!
(setq graphviz-dot-view-command "open -a /Applications/Graphviz.app %s")

(provide 'graphviz-config)
