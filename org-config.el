(setq load-path (cons "~/.emacs.d/ext/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/ext/org-mode/contrib/lisp" load-path))
(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Habit mode is incredible
(add-to-list 'org-modules 'org-habit)

;; Calculate statistics for everything in the subtree
(setq org-hierarchical-todo-statistics nil)

;; Configure for MobileOrg
(require 'org-mobile)
(setq org-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-mobile-files '("~/Dropbox/org"))
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull (concat org-directory "/from-inbox.org"))
(setq org-enforce-todo-dependencies t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)" "APPT(a)" "|" "DONE(d!)" "CANCELLED(c@)" "DEFERRED(f@)")))

;; Agenda Tweaks
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

(setq org-agenda-include-diary nil)

(setq org-log-done 'note)
(setq org-log-into-drawer t)

;; Set up hooks
(defun my-org-mode-hook ()
  (progn
    (visual-line-mode +1)))
(add-hook 'org-mode-hook 'my-org-mode-hook)

;; Stole this next bit from the INFO pages
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "<f9>") 'org-mobile-push)
(global-set-key (kbd "S-<f9>") 'org-mobile-pull)

(setq org-default-notes-file (concat org-directory "/inbox.org"))
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("g" "Groceries" entry
         (file+headline "~/Dropbox/org/shopping.org" "Groceries")
         "* TODO %? %^G\n")
        ("s" "General Shopping" entry
         (file+headline "~/Dropbox/org/shopping.org" "Other Things To Buy")
         "* TODO %? %^G\n")
        ("e" "Miscellaneous Code Changes for ENCODE project" entry
         (file+headline "~/Dropbox/org/encode.org" "Code Changes")
         "* TODO %? :programming:\n")
        ("t" "General TODO" entry
         (file+headline "" "Tasks")
         "* TODO %? %^G\n SCHEDULED: %^t\n %i\n")))

(provide 'org-config)
