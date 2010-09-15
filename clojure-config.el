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

(require 'swank-clojure-extra)
;; For some reason it's not finding lein on my path...
(setq swank-clojure-lein-swank-command "/Users/maier/bin/lein")

;; Stolen from http://github.com/vu3rdd/swank-clojure-extra
(eval-after-load "slime"
  '(progn
     (require 'swank-clojure-extra)
     (add-to-list 'slime-lisp-implementations `(clojure ,(swank-clojure-cmd)
							:init swank-clojure-init)
		  t)
     (add-hook 'slime-indentation-update-hooks 'swank-clojure-update-indentation)
     (add-hook 'slime-repl-mode-hook 'swank-clojure-slime-repl-modify-syntax t)
     (add-hook 'clojure-mode-hook 'swank-clojure-slime-mode-hook t)))

(eval-after-load 'slime
  '(setq slime-protocol-version 'ignore))

(add-hook 'clojure-mode-hook
  '(lambda ()
     ;; Fix whitespace errors, save, and compile instead of just saving
     (define-key clojure-mode-map [remap save-buffer] (lambda ()
                                                        (interactive)
                                                        (whitespace-cleanup)
                                                        (save-buffer)
                                                        (slime-compile-and-load-file)))
     ;; Handy key for hooking up to swank
     (define-key clojure-mode-map [f8] 'swank-clojure-lein-swank)))

(provide 'clojure-config)
