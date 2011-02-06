(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; (defun javascript-mode-config ()
;;   (highlight-parentheses-mode t))

;; (add-hook 'javascript-mode-hook 'javascript-mode-config)

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(autoload 'js2-mode "js2-mode" "Start JS2 Mode" t)

(defun js2-custom-setup ()
  (moz-minor-mode 1)
  (setq autopair-dont-activate t))

(add-hook 'js2-mode-hook 'js2-custom-setup)

(provide 'javascript-config)
