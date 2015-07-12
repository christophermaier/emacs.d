(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

(setq-default
 inhibit-startup-screen t
 initial-scratch-message ""
 initial-major-mode 'org-mode
 inhibit-startup-message t)

(provide 'init-frames)
