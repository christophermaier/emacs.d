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

(setq org-clock-out-remove-zero-time-clocks t)
(setq org-deadline-warning-days 3)

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

(add-hook 'org-mode-hook
          (let ((original-command (lookup-key org-mode-map [tab])))
            `(lambda ()
               (setq yas/fallback-behavior
                     '(apply ,original-command))
               (local-set-key [tab] 'yas/expand))))



;; Save clock history
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;; YES! Use Ido!
(setq org-completion-use-ido t)
(setq org-outline-path-complete-in-steps t)

;; Allow refiling a task to a new top-level item in a file
(setq org-refile-use-outline-path 'file)

(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-hide-leading-stars t)


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
(global-set-key (kbd "<f11>") 'org-agenda-clock-in)
(global-set-key (kbd "<f12>") 'org-agenda-clock-out)

(setq org-default-notes-file (concat org-directory "/inbox.org"))
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("o" "Organization and Planning")
        ("oe" "Emacs Setup and Tweaking" entry
         (file+headline "~/Dropbox/org/programming.org" "Emacs Configuration")
         "* TODO %?")
        ("oo" "Org-Mode Setup and Tweaking" entry
         (file+headline "~/Dropbox/org/programming.org" "Org-Mode Configuration")
         "* TODO %?")

        ("s" "Shopping")
        ("sg" "Groceries" entry
         (file+headline "~/Dropbox/org/shopping.org" "Groceries")
         "* TODO %? %^G\n")
        ("ss" "General Shopping" entry
         (file+headline "~/Dropbox/org/shopping.org" "Other Things To Buy")
         "* TODO %? %^G\n")

        ("t" "General TODO" entry
         (file+headline "" "Tasks")
         "* TODO %? %^G\n SCHEDULED: %^t\n %i\n")

        ("w" "Work Templates")
        ("we" "Miscellaneous Code Changes for ENCODE project" entry
         (file+headline "~/Dropbox/org/encode.org" "Code Changes")
         "* TODO %? :programming:\n")

        ("x" "Templates for Expenses")
        ("xg" "Log a Gift Expense" table-line
         (file+headline "~/Dropbox/org/financial.org" "Reconcile Gift Expenses From Last Month")
         "|%t|%?||"
         :table-line-pos "II-1")
        ("xi" "Log an Infrequent Expense" table-line
         (file+headline "~/Dropbox/org/financial.org" "Reconcile Irregular Expenses From Last Month")
         "|%t|%?||"
         :table-line-pos "II-1")))

(setq org-agenda-custom-commands 
      '(("w" "Things I'm Waiting On" todo "WAITING")
        ("e" "Errands" tags-todo "errands|shopping")
        ("s" "Scheduled for Today" agenda ""
         (
          (org-agenda-entry-types '(:scheduled))
          (org-agenda-sorting-strategy '(time-up habit-up category-up tag-down))))))

(setq org-refile-targets '((org-agenda-files . (:level . 1))))

(provide 'org-config)
