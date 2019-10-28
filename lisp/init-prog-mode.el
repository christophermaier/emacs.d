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
(require-package 'highlight-symbol)
(add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
(with-eval-after-load 'highlight-symbol
  ;; M-n and M-p already do next / prev symbol navigation
  (define-key highlight-symbol-nav-mode-map (kbd "M-N") 'highlight-symbol-next-in-defun)
  (define-key highlight-symbol-nav-mode-map (kbd "M-P") 'highlight-symbol-prev-in-defun)

  (define-key highlight-symbol-nav-mode-map (kbd "<f9>") 'highlight-symbol)
  (define-key highlight-symbol-nav-mode-map (kbd "C-<f9>") 'highlight-symbol-query-replace))
;; TODO: try and cook up a highlight-symbol-query-replace-in-defun

(provide 'init-prog-mode)
