(require-package 'projectile)
(require-package 'helm-projectile)

(with-eval-after-load 'projectile
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-use-git-grep t))

;; Some key commands:
;; C-c p p -> switch project
;; C-c p h -> helm!
;; C-c p t -> toggle with test file (doesn't seem to go back to impl, though)
;; C-c p k -> kill current project buffers
;; C-c p S -> save current project buffers

(provide 'init-projectile)
