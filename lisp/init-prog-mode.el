(defun cwmaier/auto-fill-comments ()
  "Programming mode hook to enable auto-fill only in comments"
  (setq comment-auto-fill-only-comments t)
  (auto-fill-mode))
(add-hook 'prog-mode-hook 'cwmaier/auto-fill-comments)

;; Use rainbow-delimiters only to highlight unbalanced delimiters
;; Stolen from http://timothypratley.blogspot.com/2015/07/seven-specialty-emacs-settings-with-big.html
(require-package 'rainbow-delimiters)
(setq-default rainbow-delimiters-max-face-count 1)
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground 'unspecified
                    :inherit 'error)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Column Markers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require-package 'column-marker)
;; (defun cwmaier/mark-columns ()
;;   (interactive)
;;   (column-marker-1 72)
;;   (column-marker-2 80)
;;   (column-marker-3 120))
;; (add-hook 'prog-mode-hook 'cwmaier/mark-columns)

;; highlight-symbol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-nav-mode)
  :bind (:map highlight-symbol-nav-mode-map
              ("M-N" . highlight-symbol-next-in-defun)
              ("M-P" . highlight-symbol-prev-in-defun)
              ("<f9>" . highlight-symbol)
              ("C-<f9>" . highlight-symbol-query-replace)))

(provide 'init-prog-mode)
