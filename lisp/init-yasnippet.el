(require-package 'yasnippet)
(require-package 'helm-c-yasnippet)

(with-eval-after-load 'yasnippet
  (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
  (yas-global-mode 1)
  (diminish 'yas-minor-mode))

(with-eval-after-load 'helm-c-yasnippet
  (setq helm-yas-space-match-any-greedy t)
  (global-set-key (kbd "C-c y") 'helm-yas-complete))

(provide 'init-yasnippet)
