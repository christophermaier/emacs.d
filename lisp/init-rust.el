(when (executable-find "rustc")
  (require-package 'rust-mode)
  (require-package 'cargo)
  (require-package 'flycheck-rust)
  (require-package 'toml-mode)
  (require-package 'racer)

  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)

  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

(provide 'init-rust)
