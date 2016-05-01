(when (executable-find "go")
  (require-package 'go-mode)
  (require-package 'company-go)
  (require-package 'flycheck-gometalinter)
  (require-package 'go-projectile))

(provide 'init-go)
