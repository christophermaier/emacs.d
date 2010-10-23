(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq load-path (cons "~/.emacs.d/ext/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/ext/org-mode/contrib/lisp" load-path))
(require 'org)
(require 'org-mobile)

(setq org-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-mobile-files '("~/Dropbox/org"))
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull (concat org-directory "/from-inbox.org"))
(setq org-enforce-todo-dependencies t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "APPT(a)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(f)")))

(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

(setq org-log-done 'note)

(defun my-org-mode-hook ()
  (progn
    (visual-line-mode +1)))
(add-hook 'org-mode-hook 'my-org-mode-hook)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "<f9>") 'org-mobile-push)
(global-set-key (kbd "S-<f9>") 'org-mobile-pull)

(setq org-default-notes-file (concat org-directory "/inbox.org"))
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/inbox.org") "Tasks")
         "* TODO %?\n %i\n %a")))

(provide 'org-config)
