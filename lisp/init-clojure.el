(require-package 'clojure-mode)
(require-package 'cider)
(require-package 'flycheck-clojure)

;; May not be working just yet
(with-eval-after-load 'flycheck-clojure
  (eval-after-load 'flycheck '(flycheck-clojure-setup))
;;  (add-to-list 'flycheck-checkers 'clojure-cider-eastwood) ;; linting
;;  (add-to-list 'flycheck-checkers 'clojure-cider-kibit)
  )   ;; code structure

(add-hook 'cider-repl-mode-hook 'cwmaier/setup-lisp)

(provide 'init-clojure)
