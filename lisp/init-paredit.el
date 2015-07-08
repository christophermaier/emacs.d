(require-package 'paredit)
(diminish 'paredit-mode "()")

(require-package 'paredit-everywhere)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)
(diminish 'paredit-everywhere-mode "()-")

(provide 'init-paredit)
