(add-hook-to-modes
 lisp-modes
 enable-paredit-mode)

  ;; (lambda ()
  ;;  (show-paren-mode t)
  ;;  (paredit-mode t)
  ;;  (local-set-key (kbd "C-c (") 'paredit-backward-slurp-sexp)
  ;;  (local-set-key (kbd "C-c )") 'paredit-forward-slurp-sexp)
  ;;  (local-set-key (kbd "C-c 9") 'paredit-backward-barf-sexp)
  ;;  (local-set-key (kbd "C-c 0") 'paredit-forward-barf-sexp)))))
