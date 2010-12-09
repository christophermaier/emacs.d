(add-to-list 'load-path "~/.emacs.d/ext/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/ext/yasnippet-0.6.1c/snippets")

(provide 'yasnippet-config)
