(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ext/auto-complete/dict")
(ac-config-default)

(setq ac-dwim t)

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

(provide 'ac-config)
