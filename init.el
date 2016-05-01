(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(set-frame-font "Source Code Pro-16" nil t)

(defconst *is-a-mac* (eq system-type 'darwin) "True if running on OS X")

(require 'init-utils)
(require 'init-package)
(require-package 'diminish)
(require-package 'fullframe)

(require 'init-exec-path)
(require 'init-frames)
(require 'init-windows)
(require 'init-editing)

(require 'init-helm)
(require 'init-projectile)
(require 'init-company)

(require 'init-git)

(require 'init-prog-mode)
(require 'init-erlang)
(require 'init-ruby)
(require 'init-paredit)
(require 'init-lisp)
(require 'init-clojure)
(require 'init-rust)
(require 'init-go)
(require 'init-yaml)
(require 'init-yasnippet)

(require 'init-spelling)
(require 'init-eshell)
(require 'init-org)

(require 'init-dot)

;; (require-package 'hc-zenburn-theme)

(when (executable-find "gnuplot")
  (require-package 'gnuplot))

;; Stolen from Steve Purcell
(global-set-key (kbd "C-h K") 'find-function-on-key)

(require 'server)
(unless (server-running-p)
  (server-start))
