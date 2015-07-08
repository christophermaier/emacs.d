(require-package 'company)
(global-company-mode)
(diminish 'company-mode)
(setq-default company-idle-delay 0.2)

;; Helm has spoiled me; can't use M-n / M-p anymore :)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

(provide 'init-company)
