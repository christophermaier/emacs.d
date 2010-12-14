;; Load up all my config files and libraries
;; This requires Emacs 23 because of user-emacs-directory
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; ELPA
(load "package")
(package-initialize)
(require 'elpa-config)

;; My various configuration files


(require 'ac-config)
(require 'yasnippet-config)
(require 'miscellaneous-config)
(require 'org-config)
(require 'ido-config)
;;(require 'smart-tab-config)
(require 'autopair-config)
(require 'color-theme-config)
(require 'highlight-parens-config)
(require 'clojure-config)
(require 'slime-config)
(require 'emacs-lisp-config)
(require 'javascript-config)
(require 'markdown-config)
(require 'whitespace-config)
(require 'magit-config)
(require 'haskell-config)
(require 'epresent-config)
(require 'htmlize-config)
(require 'graphviz-config)


;; This should always be last
(require 'custom-config)
