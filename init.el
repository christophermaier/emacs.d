;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This code stolen from http://github.com/bbatsov/emacs.d/blob/master/init.el
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq ext-dir (concat dotfiles-dir "ext/"))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path ext-dir)


;; ELPA
(setq package-user-dir (concat dotfiles-dir "elpa"))
(load "package")
(package-initialize)

(require 'elpa-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; My various configuration files

(require 'miscellaneous-config)

(require 'ido-config)
;;; Can't get smart-tab behaving for me in Clojure's SLIME REPL yet :(
(require 'smart-tab-config)
(require 'autopair-config)
(require 'color-theme-config)
(require 'highlight-parens-config)
(require 'clojure-config)
(require 'emacs-lisp-config)
(require 'javascript-config)
(require 'markdown-config)
(require 'whitespace-config)
