;; Just how I want things for editing Clojure code

;; Let's turn on Paredit by default, shall we?
(add-hook 'clojure-mode-hook
      (lambda ()
        (paredit-mode +1)))

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

(require 'elein)
(define-key clojure-mode-map [f8] 'elein-swank)
(define-key clojure-mode-map [S-f8] 'elein-kill-swank)
(define-key clojure-mode-map [M-f8] 'elein-reswank)

(require 'clojure-refactoring-mode)

(provide 'clojure-config)
