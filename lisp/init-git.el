(when (executable-find "git")
  (require-package 'magit)
  (with-eval-after-load 'magit
    (global-set-key [f7] 'magit-status))

  (require-package 'github-browse-file)
  (with-eval-after-load 'github-browse-file
    (global-set-key [f8] 'github-browse-file))

  (require-package 'git-gutter-fringe)
  (with-eval-after-load 'git-gutter-fringe
    (global-git-gutter-mode t)
    (diminish 'git-gutter-mode)
    (setq git-gutter-fr:side 'left-fringe)))

(provide 'init-git)
