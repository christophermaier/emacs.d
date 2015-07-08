(require-package 'helm)
(require-package 'helm-ls-git)
(require-package 'helm-swoop)

(with-eval-after-load 'helm
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)

  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  ;; Normally `C-x m` is bound to `compose-mail`, but I never use that
  ;; `C-c m` isn't bound to anything; I'll set both here and see which
  ;; I like best :P
  (global-set-key (kbd "C-x m") 'helm-bookmarks)
  (global-set-key (kbd "C-c m") 'helm-bookmarks))

(with-eval-after-load 'helm-ls-git
  ;; Another fun thing is =C-]=, which toggles full paths on and off.
  (global-set-key (kbd "C-x C-d") 'helm-browse-project))

(provide 'init-helm)
