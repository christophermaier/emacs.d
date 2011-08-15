;; This is where a vanilla Erlang install from source code goes on OS X
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq erlang-version "2.6.6.4")

;; These configurations are taken from 
;; http://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html
(setq load-path
      (cons (concat erlang-root-dir "/lib/tools-" erlang-version "/emacs")
            load-path))
(setq exec-path
      (cons (concat erlang-root-dir "/bin")
            exec-path))

(require 'erlang-start)

(provide 'erlang-config)
