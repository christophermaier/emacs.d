;; Stolen directly from Steve Purcell (with a tiny tweak):
;; https://github.com/purcell/emacs.d/blob/7f47e64a83a87134b12755464407a30711ff5eb3/lisp/init-exec-path.el
(require-package 'exec-path-from-shell)

(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
