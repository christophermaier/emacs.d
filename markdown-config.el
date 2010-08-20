(require 'markdown-mode)

;;; You'll need to have the 'markdown' Perl script installed somewhere
;;; on your path.  Mine's at /usr/bin/markdown.  It also needs to be named
;;; "markdown", since that's what markdown-mode looks for by default.
;;; (that's configurable, though)

;;; Download markdown at http://daringfireball.net/projects/markdown/

;;; There's a bunch of different extensions people use for Markdown docs; register them all
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; I want to treat files named "README" should be treated as Markdown files
(add-to-list 'auto-mode-alist '("README\\'" . markdown-mode))

(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode +1)))

(provide 'markdown-config)
