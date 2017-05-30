;; (require 'org-mobile)
(require 'org-habit)

(setq org-directory "~/Dropbox/org")

(defun org-file (filename-without-extension)
  (concat org-directory "/" filename-without-extension ".org"))

(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

(add-hook 'org-mode-hook 'turn-on-flyspell 'append)
(add-hook 'org-mode-hook 'auto-fill-mode 'append)

;; Stole this next bit from the INFO pages
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;;(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cc" 'helm-org-capture-templates)
(global-set-key "\C-cl" 'org-store-link)

;; (global-set-key (kbd "<f9>") 'org-mobile-push)
;; (global-set-key (kbd "S-<f9>") 'org-mobile-pull)

(defun my/work-log ()
  "Capture a work log item; see org-capture-templates"
  (interactive)
  (org-capture nil "wl"))
(global-set-key (kbd "<f10>") 'my/work-log)

(global-set-key (kbd "<f11>") 'org-agenda-clock-in)
(global-set-key (kbd "<f12>") 'org-agenda-clock-out)

(define-key org-agenda-mode-map (kbd "C->") 'org-agenda-date-later-hours)
(define-key org-agenda-mode-map (kbd "C-<") 'org-agenda-date-earlier-hours)

;; Basic Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq
 org-directory "~/Dropbox/org"
 org-blank-before-new-entry nil
;; org-mobile-files `(,org-directory)
;; org-mobile-directory "~/Dropbox/MobileOrg"
;; org-mobile-inbox-for-pull (org-file "from-inbox")
 org-agenda-files `(,org-directory "~/Dropbox/org/projects")
 org-agenda-span 'day
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
 org-agenda-include-diary nil
 org-agenda-log-mode-items '(closed clock)
 org-enforce-todo-dependencies t
 org-todo-keywords '((sequence "TODO(t)"
                               "STARTED(s!)"
                               "WAITING(w@/!)"
                               "APPT(a)"
                               "|"
                               "DONE(d!)"
                               "CANCELLED(c@)"
                               "DEFERRED(f@)"))
 org-treat-S-cursor-todo-selection-as-state-change nil
 org-use-fast-todo-selection t
 org-use-property-inheritance t
 org-refile-use-outline-path 'file
 org-refile-allow-creating-parent-nodes 'confirm
 org-refile-targets '((org-agenda-files . (:maxlevel . 5))
                      (nil . (:maxlevel . 5)))
 org-clock-out-remove-zero-time-clocks t
 org-clock-persist t
 org-completion-use-ido t
 org-deadline-warning-days 3
 org-hide-leading-stars t
 org-log-done 'note
 org-log-into-drawer t
 org-outline-path-complete-in-steps t
 org-hierarchical-todo-statistics nil
 org-src-fontify-natively t
 org-src-tab-acts-natively t
 org-src-window-setup 'current-window
 org-use-sub-superscripts '{}
 org-habit-graph-column 60
 org-startup-with-inline-images t)

;; Agenda Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default
 org-agenda-custom-commands '(("p" . "Priorities")
                              ("pa" "A items" tags-todo "+PRIORITY=\"A\""
                               ((org-agenda-todo-ignore-scheduled 'future)
                                (org-agenda-tags-todo-honor-ignore-options t)))
                              ("pb" "B items" tags-todo "+PRIORITY=\"B\""
                               ((org-agenda-todo-ignore-scheduled 'future)
                                (org-agenda-tags-todo-honor-ignore-options t)))
                              ("pc" "C items" tags-todo "+PRIORITY=\"C\""
                               ((org-agenda-todo-ignore-scheduled 'future)
                                (org-agenda-tags-todo-honor-ignore-options t)))

                              ("s" . "Shopping Lists")
                              ("sa" "Alex"               tags-todo "alex&shopping")
                              ("sb" "My Books"           tags-todo "chris&book&shopping")
                              ("sc" "Me"                 tags-todo "chris&shopping-books")
                              ("sd" "Dee"                tags-todo "dee&shopping")
                              ("sx" "Xmas Shopping"      tags-todo "xmas&shopping")

                              ("W" "Waiting" todo "WAITING")

                              ("e" "Errands" tags-todo "errands|shopping"
                               ((org-agenda-todo-ignore-scheduled 'future)
                                (org-agenda-tags-todo-honor-ignore-options t)))

                              ("r" "Refile" tags "+REFILE")

                              ("h" "Habits Only"
                               ((agenda "Habits" ((org-agenda-sorting-strategy '(habit-up time-up))))))

                              ("f" "Financial Work" agenda ""
                               ((org-agenda-files `(,(org-file "financial")))))

                              ("w" . "Work Stuff")
                              ("we" "Work" agenda ""
                               ((org-agenda-files `(,(org-file "work")))
                                (org-agenda-sorting-strategy '(priority-down effort-down))))

                              ("wm" "1-on-1 with my manager" tags-todo "manager_1on1")
                              ("wp" "Principals Meeting" tags-todo "principals")
                              ("wz" "Upcoming work deadlines - next 10 days" agenda ""
                               ((org-agenda-files `(,(org-file "work")))
                                (org-agenda-time-grid nil)
                                (org-deadline-warning-days 10)
                                (org-agenda-entry-types '(:deadline))
                                (org-agenda-sorting-strategy '(deadline-up))))

                              ("x" "Added and undone in the past week" tags-todo "+Added>=\"<-7d>\"")

                              ("y" "Upcoming personal deadlines - next 10 days" agenda ""
                               ((org-agenda-files `(,@(delete (file-truename (org-file "work"))
                                                              (org-agenda-files))))
                                (org-agenda-time-grid nil)
                                (org-deadline-warning-days 10)
                                (org-agenda-entry-types '(:deadline))
                                (org-agenda-sorting-strategy '(deadline-up))))

                              ("z" "Appointments Today" agenda*)))

;; Capture Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default
 org-default-notes-file (org-file "inbox")
 org-capture-templates `(
                         ("j" "Daily Journal" entry
                          (file+datetree (org-file "review"))
                          "* %U - %^{Activity}\n  %?")

                         ("s" "Shopping")
                         ("sg" "Groceries" entry
                          (file+headline (org-file "shopping")
                                         "Groceries")
                          "* TODO %? %^G\n")
                         ("ss" "General Shopping" entry
                          (file+headline (org-file "shopping")
                                         "Other Things To Buy")
                          "* TODO %? %^G\n")

                         ("t" "New TODO")
                         ("tc" "Computer TODO" entry
                          (file+headline (org-file "projects/computer") "General Computer Setup")
                          "* TODO %^{Task}\n  :PROPERTIES:\n  :Added: %U\n  :END:"
                          :prepend t
                          :immediate-finish t)
                         ("te" "Emacs TODO" entry
                          (file+headline (org-file "projects/computer") "Emacs")
                          "* TODO %^{Task}\n  :PROPERTIES:\n  :Added: %U\n  :END:"
                          :prepend t
                          :immediate-finish t)
                         ("tt" "TODO For today" entry
                          (file org-default-notes-file)
                          "* TODO %^{Activity}\n  DEADLINE: <%<%Y-%m-%d %a 23:59>>\n  :PROPERTIES:\n  :Added: %U\n  :END:"
                          :prepend t)
                         ("tu" "TODO for tomorrow" entry
                          (file org-default-notes-file)
                          "* TODO %^{Activity}\n  DEADLINE: <%(cwmaier/org-date (calendar-current-date 1)) 23:59>\n  :PROPERTIES:\n  :Added: %U\n  :END:"
                          :prepend t)
                         ("tw" "TODO for this weekend" entry
                          (file org-default-notes-file)
                          "* TODO %^{Activity}\n  SCHEDULED: <%(cwmaier/org-date (cwmaier/this-weekends-saturday))> DEADLINE: <%(cwmaier/org-date (cwmaier/this-weekends-sunday))>\n  :PROPERTIES:\n  :Added: %U\n  :END:")
                         ("tx" "TODO sometime in the next week" entry
                          (file org-default-notes-file)
                          "* TODO %^{Activity}\n  SCHEDULED: <%<%Y-%m-%d %a>> DEADLINE: <%(cwmaier/org-date (cwmaier/one-week-from-today))>\n  :PROPERTIES:\n  :Added: %U\n  :END:"
                          :prepend t)

                         ("w" "Work")
                         ,(my/agenda-item "wm" "1-on-1 Agenda Item"
                                          (org-file "work")
                                          "Manager 1-on-1 Meeting Agenda Items")
                         ("wl" "Work Log" entry
                          (file+datetree (org-file "work_log"))
                          "* %U - %^{Activity}\n  %?")

                         ,(my/agenda-item "wp" "Principals Meeting Agenda Item"
                                          (org-file "work")
                                          "Principals Meeting Agenda Items")
                         ("wt" "Work TODO today" entry
                          (file (org-file "work"))
                          "* TODO %^{Activity}\n  DEADLINE: <%<%Y-%m-%d %a 17:00>>\n  :PROPERTIES:\n  :Added: %U\n  :END:"
                          :prepend t)
                         ("wu" "Work TODO for tomorrow" entry
                          (file (org-file "work"))
                          "* TODO %^{Activity}\n  DEADLINE: <%(cwmaier/org-date (calendar-current-date 1)) 17:00>\n  :PROPERTIES:\n  :Added: %U\n  :END:"
                          :prepend t)
                         ("wx" "Work TODO sometime in the next week" entry
                          (file (org-file "work"))
                          "* TODO %^{Activity}\n  SCHEDULED: <%<%Y-%m-%d %a>> DEADLINE: <%(cwmaier/org-date (cwmaier/one-week-from-today)) 17:00>\n  :PROPERTIES:\n  :Added: %U\n  :END:"
                          :prepend t)))

(org-clock-persistence-insinuate)

(defun cwm-work-tasks-done-last-workday ()
  "Produces an org agenda tags view list of the work tasks
    completed on the last work day (i.e., yesterday, or last Friday
    if today is Monday or Sunday).  Good for daily stand-up meetings."
  (interactive)
  (let* ((day-of-week (calendar-day-of-week (calendar-current-date)))
         (start-day (calendar-current-date (cond ((= day-of-week 1) -3)     ;; if Monday, go back 3 days to Friday
                                                 ((= day-of-week 0) -2)     ;; if Sunday, go back 2 days to Friday
                                                 (t                 -1))))  ;; otherwise, use yesterday
         (end-day (calendar-current-date (cond ((= day-of-week 1) -2)       ;; if Monday, go back 2 days to Saturday
                                               ((= day-of-week 0) -1)       ;; if Sunday, go back 1 day to Saturday
                                               (t                  0)))))   ;; otherwise, use today
    (org-tags-view nil
                   (concat "CATEGORY=\"work\"+TODO=\"DONE\""
                           (format "+CLOSED>=\"[%d-%02d-%02d]\""
                                   (calendar-extract-year start-day)
                                   (calendar-extract-month start-day)
                                   (calendar-extract-day start-day))
                           (format "+CLOSED<=\"[%d-%02d-%02d]\""
                                   (calendar-extract-year end-day)
                                   (calendar-extract-month end-day)
                                   (calendar-extract-day end-day))))))
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;;(clojure . t)
   ;;(sh . t)
   ;;(dot . t)
   ;;(js . t)
   (gnuplot . t)))
(require-package 'gnuplot)
(provide 'init-org)
