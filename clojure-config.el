;; Just how I want things for editing Clojure code

;; Let's turn on Paredit by default, shall we?
(add-hook 'clojure-mode-hook
      (lambda ()
        (paredit-mode +1)))

;; Paredit would be sweet to have in the REPL, too
;; Strictly speaking, this could probably go somewhere other than a Clojure
;; configuration file, but I currently don't do any REPL work in anything
;; but Clojure, so here it stays
(add-hook 'slime-repl-mode-hook
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

(add-hook 'clojure-mode-hook
  '(lambda ()
     ;; Fix whitespace errors, save, and compile instead of just saving
     (define-key clojure-mode-map [remap save-buffer] (lambda ()
                                                        (interactive)
                                                        (whitespace-cleanup)
                                                        (save-buffer)
                                                        (slime-compile-and-load-file)))
     ;; Handy key for hooking up to swank
     (define-key clojure-mode-map [f8] 'slime-connect)))

(provide 'clojure-config)
