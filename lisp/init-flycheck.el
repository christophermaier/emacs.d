(require-package 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)


;; Really tired of seeing all the Emacs Lisp documentation checks
(setq-default flycheck-disabled-checkers
              '(emacs-lisp-checkdoc))

;; HELM!!!
;; (Though it's not toooooo terribly different from just running
;;   C-c ! l (flycheck-list-errors)
;;
;; Overall consistency is good, though, I suppose
(require-package 'helm-flycheck)
(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(provide 'init-flycheck)
