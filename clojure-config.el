;; Just how I want things for editing Clojure code

;;; Fix indentation for Lazytest functions (as per http://github.com/stuartsierra/lazytest)
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (describe 'defun)
     (it 'defun)
     (given 'defun)
     (defmulti 'defun)
     (defmethod 'defun)))

(defun clojure-hook-setup ()
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))

(add-hook 'clojure-mode-hook 'clojure-hook-setup)


(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

(require 'durendal)
(durendal-enable)

(global-set-key "\C-cs" 'slime-selector)

(require 'clojure-refactoring-mode)

(provide 'clojure-config)
