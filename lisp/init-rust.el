(when (executable-find "rustc")
  (require-package 'rust-mode)
  (require-package 'flycheck-rust)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'init-rust)
