;; So this is fun...
;;
;; It is recommended that company-mode is started with an
;; after-init-hook, which means that anything that calls on
;; company-mode during init will probably fail (unless it's required
;; first). You can diminish it in the after-init-hook as well, but you
;; have to add it to the hook *before* you add the global-company-mode
;; call, because adding to the hook adds it to the front of the list!
;;
;; Might as well just call global-company-mode directly, since that'll
;; autoload things, and just avoid the hook altogether. Not like this
;; is going to increase startup times, since it's going to require
;; company-mode anyway.

;; (defun diminish-company ()
;;   (diminish 'company-mode))
;; (add-hook 'after-init-hook 'diminish-company)
;; (add-hook 'after-init-hook 'global-company-mode)

(global-company-mode)
(diminish 'company-mode)
