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
 '(default ((t (:inherit nil
		:stipple nil
		:background "#3f3f3f"
		:foreground "#dcdccc"
		:inverse-video nil
		:box nil
		:strike-through nil
		:overline nil
		:underline nil
		:slant normal
		:weight normal
		:height 140
		:width normal
		:foundry "apple"
		:family "Andale Mono")))))
