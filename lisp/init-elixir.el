(when (executable-find "elixir")
  (require-package 'alchemist)
  (setq alchemist-hooks-compile-on-save t))

(provide 'init-elixir)
