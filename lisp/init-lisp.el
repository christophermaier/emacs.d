;; TODO: Consider activating this for all programming modes
(require-package 'hl-sexp)

;; Stolen from Steve Purcell
(defun sanityinc/maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))
(add-hook 'emacs-lisp-mode-hook 'sanityinc/maybe-set-bundled-elisp-readonly)

(defun cwmaier/setup-lisp ()
  (enable-paredit-mode)
  (hl-sexp-mode 1))

(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook))
  (add-hook hook 'cwmaier/setup-lisp))

(provide 'init-lisp)
