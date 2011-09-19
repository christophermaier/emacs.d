;; Just how I want things for editing Clojure code

;;; Fix indentation for Lazytest functions (as per http://github.com/stuartsierra/lazytest)
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (describe 'defun)
     (it 'defun)
     (given 'defun)))

(defun clojure-hook-setup ()
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))

(add-hook 'clojure-mode-hook 'clojure-hook-setup)

(require 'clojure-refactoring-mode)

(provide 'clojure-config)
