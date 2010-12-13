(add-to-list 'load-path "~/.emacs.d/ext/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)

(setq yas/root-directory '("~/.emacs.d/snippets"
                           "~/.emacs.d/ext/yasnippet-0.6.1c/snippets"))
(mapc 'yas/load-directory yas/root-directory)

(provide 'yasnippet-config)
