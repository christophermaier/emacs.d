(autoload 'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

;; (defun javascript-mode-config ()
;;   (highlight-parentheses-mode t))

;; (add-hook 'javascript-mode-hook 'javascript-mode-config)


(provide 'javascript-config)
