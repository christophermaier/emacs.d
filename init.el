;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
(require 'init-flycheck)
(require 'init-helm)
(require 'init-projectile)
(require 'init-company)

(require 'init-git)

(require 'init-prog-mode)
(require 'init-erlang)
(require 'init-elixir)
(require 'init-ruby)
(require 'init-paredit)
(require 'init-lisp)
(require 'init-clojure)
(require 'init-rust)
(require 'init-go)
(require 'init-yaml)
(require 'init-yasnippet)
(require 'init-terraform)

(require 'init-spelling)
(require 'init-eshell)
(require 'init-org)

(require 'init-dot)

(require-package 'hc-zenburn-theme)
(require-package 'protobuf-mode)

(when (executable-find "gnuplot")
  (require-package 'gnuplot))

;; Stolen from Steve Purcell
(global-set-key (kbd "C-h K") 'find-function-on-key)

(require 'server)
(unless (server-running-p)
  (server-start))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-commit-arguments (quote ("--signoff")))
 '(package-selected-packages
   (quote
    (flycheck-color-mode-line flycheck helm-flycheck racer cargo protobuf-mode toml-mode graphviz-dot-mode gnuplot terraform-mode helm-c-yasnippet yasnippet yaml-mode flycheck-clojure cider clojure-mode hl-sexp paredit-everywhere paredit ruby-tools ruby-mode highlight-symbol column-marker rainbow-delimiters git-gutter-fringe github-browse-file magit company helm-projectile projectile helm-descbinds helm-swoop helm-ls-git helm exec-path-from-shell fullframe diminish))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
