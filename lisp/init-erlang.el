(when (executable-find "erl")
  (require-package 'erlang)

  (when (package-installed-p 'erlang)
    (require 'erlang-start)))

(provide 'init-erlang)
