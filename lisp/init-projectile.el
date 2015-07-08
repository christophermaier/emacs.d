(require-package 'projectile)
(require-package 'helm-projectile)

(with-eval-after-load 'projectile
  (projectile-global-mode))

;; Some key commands:
;; C-c p p -> switch project
;; C-c p h -> helm!
;; C-c p t -> toggle with test file (doesn't seem to go back to impl, though)
;; C-c p k -> kill current project buffers
;; C-c p S -> save current project buffers

(provide 'init-projectile)
