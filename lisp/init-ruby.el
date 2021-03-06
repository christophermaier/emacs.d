(when (executable-find "ruby")
  (require-package 'ruby-mode)
  (require-package 'ruby-tools)

  (with-eval-after-load 'ruby-mode
    (add-to-list 'auto-mode-alist '("Berksfile" . ruby-mode))
    (add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
    (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
    (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
    (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
    (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
    (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))))

(provide 'init-ruby)
