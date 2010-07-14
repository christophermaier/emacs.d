;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This code stolen from http://github.com/bbatsov/emacs.d/blob/master/init.el
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq ext-dir (concat dotfiles-dir "ext/"))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path ext-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; My various configuration files

(require 'miscellaneous-config)

(require 'ido-config)
(require 'smart-tab-config)
(require 'autopair-config)
(require 'color-theme-config)
