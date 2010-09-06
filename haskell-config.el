(load "~/.emacs.d/ext/haskell-mode-2.8.0/haskell-site-file")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-hook 'haskell-mode-hook (lambda ()
                               (setq haskell-font-lock-symbols t)))

(provide 'haskell-config)
