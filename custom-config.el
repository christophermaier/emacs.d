;; Don't pollute init.el with GUI-made customizations
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(provide 'custom-config)
