;; http://flycheck.readthedocs.org/en/latest/guide/installation.html#setup
(add-hook 'after-init-hook 'global-flycheck-mode)

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
