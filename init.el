;; Load up all my config files and libraries
;; This requires Emacs 23 because of user-emacs-directory
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; ELPA
(load "package")
(package-initialize)
(require 'elpa-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; My various configuration files

(require 'miscellaneous-config)
(require 'org-config)
(require 'ido-config)
(require 'smart-tab-config)
(require 'autopair-config)
(require 'color-theme-config)
(require 'highlight-parens-config)
(require 'clojure-config)
(require 'emacs-lisp-config)
(require 'javascript-config)
(require 'markdown-config)
(require 'whitespace-config)
(require 'magit-config)

(require 'haskell-config)
(require 'epresent-config)
(require 'htmlize-config)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((slime-port . 4005)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :family "Anonymous Pro")))))
