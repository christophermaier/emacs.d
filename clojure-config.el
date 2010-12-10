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


;; Durendal seems to refer to 'curly' paredit functions, but I only have
;; 'brace' functions... this is kind of weird, yeah?
;; At any rate, aliasing the names appears to let braces work in the REPL
;; again.
(defalias 'paredit-open-curly 'paredit-open-brace)
(defalias 'paredit-close-curly 'paredit-close-brace)

(require 'durendal)
(durendal-enable)
(define-key clojure-mode-map [f8] 'durendal-jack-in)

(global-set-key "\C-cs" 'slime-selector)

(require 'clojure-refactoring-mode)

(provide 'clojure-config)
