;; (require 'org-mobile)
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

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
 org-agenda-files `(,org-directory
                    "~/Dropbox/org/projects"
                    "~/Dropbox/org/projects/languages")
 org-agenda-span 'day
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
 org-agenda-include-diary nil
 org-agenda-log-mode-items '(closed clock)
 org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
 org-enforce-todo-dependencies t
 org-todo-keywords '((sequence "TODO(t)"
                               "NEXT(n)"
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
 org-habit-graph-column 80
 org-startup-with-inline-images t
 org-agenda-window-setup 'only-window
 org-agenda-restore-windows-after-quit t)

;; Agenda Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default
 org-agenda-custom-commands
 '(
   ("s" . "Shopping Lists")
   ("sa" "Alex"               tags-todo "alex&shopping")
   ("sb" "My Books"           tags-todo "chris&book&shopping")
   ("sc" "Me"                 tags-todo "chris&shopping-books")
   ("sd" "Dee"                tags-todo "dee&shopping")
   ("sx" "Xmas Shopping"      tags-todo "xmas&shopping")
   ("ss" "All Shopping" tags-todo "shopping"
    ((org-agenda-todo-ignore-scheduled 'future)
     (org-agenda-tags-todo-honor-ignore-options t)))

   ("c" "Chores"
    ((agenda ""
             ((org-agenda-overriding-header "Personal Chores")
              (org-agenda-files `(,(org-file "daily")))))
     (agenda ""
             ((org-agenda-overriding-header "Habits")
              (org-agenda-skip-function 'air-org-skip-subtree-if-not-habit)))
     (agenda ""
             ((org-agenda-overriding-header "Home Chores")
              (org-agenda-files `(,(org-file "maint")))))))

   ("e" "Errands" tags-todo "errands"
    ((org-agenda-todo-ignore-scheduled 'future)
     (org-agenda-tags-todo-honor-ignore-options t)))

   ("r" "Refile"
    ((tags "+REFILE"
           ((org-agenda-overriding-header "Items to refile")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'todo 'done))))
     (tags "+REFILE"
           ((org-agenda-overriding-header "Done items that REALLY need to be refiled")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'nottodo 'done)))))
    ((org-tags-match-list-sublevels 'indented)))

   ("h" "Habits" agenda ""
    ((org-agenda-overriding-header "Habits")
     (org-agenda-skip-function 'air-org-skip-subtree-if-not-habit)))

   ("f" "Financial Work" agenda ""
    ((org-agenda-files `(,(org-file "financial")))))

   ("w" . "Work Stuff")
   ("we" "Work" agenda ""
    ((org-agenda-files `(,(org-file "work")))
     (org-agenda-sorting-strategy '(time-up
                                    scheduled-up
                                    deadline-down))))

   ("wi" "Issues to File" tags-todo "issue")

   ("wm" "1-on-1 with my manager" tags-todo "manager_1on1")

   ("wr" "Team Retrospective" tags-todo "team_retrospective")

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

     ;; TODO: use deadline-up once I sort
     ;; through the really old stuff
     (org-agenda-sorting-strategy '(deadline-down))))

   ;;("z" "Appointments Today" agenda*)

   ("z" "Work Agenda"
    (
     (todo "WAITING"
           ((org-agenda-overriding-header "Waiting at Work")))
     (todo "STARTED"
           ((org-agenda-overriding-header "Started at Work")))
     (agenda ""
             ((org-agenda-overriding-header "Deadline TODAY")
              (org-agenda-skip-function
               '(org-agenda-skip-deadline-if-not-today))
              (org-agenda-entry-types '(:deadline))
              (org-agenda-sorting-strategy '(deadline-up))))
     (agenda ""
             ((org-agenda-overriding-header "Scheduled TODAY")
              (org-agenda-skip-function
               '(org-agenda-skip-scheduled-if-not-today))
              (org-agenda-entry-types '(:scheduled))))

     (tags-todo "PRIORITY=\"A\""
                ((org-agenda-overriding-header "Priority A")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-todo-ignore-scheduled 'future)
                 (org-agenda-tags-todo-honor-ignore-options t)))
     (tags-todo "PRIORITY=\"B\""
                ((org-agenda-overriding-header "Priority B")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-todo-ignore-scheduled 'future)
                 (org-agenda-tags-todo-honor-ignore-options t)))
     (tags-todo "PRIORITY=\"C\""
                ((org-agenda-overriding-header "Priority C")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-todo-ignore-scheduled 'future)
                 (org-agenda-tags-todo-honor-ignore-options t)))

     (agenda ""
             ((org-agenda-overriding-header "OVERDUE (Sometimes pathologically so)")
              (org-agenda-entry-types '(:deadline))
              (org-deadline-warning-days 0)
              ;; Ugh, this function sucks
              (org-agenda-skip-function
               '(org-agenda-skip-deadline-if-today))
              (org-agenda-sorting-strategy '(deadline-down))))
     (tags-todo "issue"
                ((org-agenda-overriding-header "Issues to file")))
     )

    (
     (org-agenda-span 'day)
     (org-agenda-files `(,(org-file "work")
                         ,(org-file "work_log")))
     )
    )

   ("q" "Test Agenda"
    (


     ;; habits
     ;; due today
     ;; scheduled today
     ;; started
     ;; next
     ;; stuck

     ;; Idea: Separate agenda views for my
     ;; areas of interest (japanese,
     ;; exercise, things to research, etc.)

     ;; TODO: actually prioritize work tasks
     (agenda ""
             ((org-agenda-overriding-header "Deadline TODAY")
              (org-agenda-span 'day)
              (org-agenda-skip-function '(org-agenda-skip-deadline-if-not-today))
              (org-agenda-entry-types '(:deadline))
              (org-agenda-sorting-strategy '(deadline-up))
              ))

     (agenda ""
             ((org-agenda-overriding-header "Scheduled TODAY")
              (org-agenda-span 'day)
              (org-agenda-skip-function '(org-agenda-skip-scheduled-if-not-today))
              (org-agenda-entry-types '(:scheduled))
              ))

     (agenda ""
             ((org-agenda-overriding-header "Scheduled, no Due Date")
              (org-agenda-span 'day)
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
              (org-agenda-sorting-strategy '(scheduled-down))
              (org-agenda-entry-types '(:scheduled))
              ))

     (todo "STARTED"
           ((org-agenda-overriding-header "Started at Home")
            ))

     ;;
     ;; Potential Improvement: Only show if
     ;; scheduled is in the past
     (agenda ""
             ((org-agenda-overriding-header "Due within 2 weeks")
              (org-agenda-entry-types '(:deadline))
              ;; This function sucks
              (org-agenda-skip-function '(org-agenda-skip-deadline-if-past))
              (org-deadline-warning-days 14)
              (org-agenda-sorting-strategy '(deadline-up))
              ))

     ;; Pathology View
     (agenda ""
             ((org-agenda-overriding-header "OVERDUE (Sometimes pathologically so)")
              (org-agenda-entry-types '(:deadline))
              (org-deadline-warning-days 0)
              ;; Ugh, this function sucks
              (org-agenda-skip-function '(org-agenda-skip-deadline-if-today))
              (org-agenda-sorting-strategy '(deadline-down))
              ))

     (tags-todo "PRIORITY=\"A\""
                ((org-agenda-overriding-header "A Priority")
                 ;; (org-agenda-skip-function
                 ;;  '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-tags-todo-honor-ignore-options t)
                 (org-agenda-todo-ignore-scheduled 'future)
                 ))

     (tags-todo "PRIORITY=\"B\""
                ((org-agenda-overriding-header "B Priority")
                 (org-agenda-todo-ignore-scheduled 'future)
                 (org-agenda-tags-todo-honor-ignore-options t)))

     (tags-todo "PRIORITY=\"C\""
                ((org-agenda-overriding-header "C Priority")
                 (org-agenda-todo-ignore-scheduled 'future)
                 (org-agenda-tags-todo-honor-ignore-options t)
                 ))

     (todo "STARTED"
           ((org-agenda-overriding-header "Currently Reading")
            (org-agenda-files `(,(org-file "to_read")))
            ))

     (stuck ""
            ((org-tags-match-list-sublevels 'indented)
             ))

     (todo "NEXT"
           ((org-agenda-overriding-header "Available Next Actions")
            (org-agenda-todo-ignore-scheduled 'future)
            ))
     )
    (
     ;; Agenda-wide Configuration
     (org-agenda-files `(,@(delete (file-truename (org-file "work_log"))
                                   (delete (file-truename (org-file "work"))
                                           (org-agenda-files)))))
     ))
   )
 )

(setq org-highest-priority ?A)
(setq org-lowest-priority ?D)
(setq org-default-priority ?D)

;; Original
;; (setq org-stuck-projects
;;       '("+LEVEL=2/-DONE" ("TODO" "NEXT" "NEXTACTION") nil ""))

;; (IDENTIFY_A_PROJECT
;;  TODO_KEYWORDS_NON_STUCK_PROJECTS
;;  TAGS_NON_STUCK_PROJECTS
;;  REGEX_NON_STUCK_PROJECTS)
(setq org-stuck-projects
      '("+project/TODO=-DONE-STARTED"
        ("NEXT")
        nil
        ""))
(setq org-agenda-dim-blocked-tasks 'invisible)
(setq org-track-ordered-property-with-tag t)









;; https://emacs.stackexchange.com/a/30194
(defun org-agenda-skip-deadline-if-not-today ()
"If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (deadline-day
            (time-to-days
              (org-time-string-to-time
                (org-entry-get nil "DEADLINE"))))
          (now (time-to-days (current-time))))
       (and deadline-day
            (not (= deadline-day now))
            subtree-end))))

(defun org-agenda-skip-deadline-if-today ()
"If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (deadline-day
            (time-to-days
              (org-time-string-to-time
                (org-entry-get nil "DEADLINE"))))
          (now (time-to-days (current-time))))
       (and deadline-day
            (= deadline-day now)
            subtree-end))))

(defun org-agenda-skip-deadline-if-past ()
"If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (deadline-day
            (time-to-days
              (org-time-string-to-time
                (org-entry-get nil "DEADLINE"))))
          (now (time-to-days (current-time))))
       (and deadline-day
            (not (> deadline-day now))
            subtree-end))))

(defun org-agenda-skip-scheduled-if-not-today ()
"If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (deadline-day
            (time-to-days
              (org-time-string-to-time
                (org-entry-get nil "SCHEDULED"))))
          (now (time-to-days (current-time))))
       (and deadline-day
            (not (= deadline-day now))
            subtree-end))))



;; Capture Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default
 org-default-notes-file (org-file "inbox")
 org-capture-templates
 `(
   ("a" "Work Project" entry
    (file+headline ,(org-file "work") "Project Log")
    "* TODO %^{Project}    :project:ORDERED:
  :PROPERTIES:
  :Added: %U
  :Issue: %^{Issue}p
  :PR:
  :PR_Submitted:
  :PR_Merged:
  :ORDERED: t
  :END:

** Description of the Situation
   %?
** Plan of Attack
** Notes
** Task List
** Issues To Spin Out of This
")

   ("c" "Capture to refile" entry
    (file ,(org-file "refile"))
    "* TODO %^{Activity}
  :PROPERTIES:
  :Added: %U
  :END:
  %?"
    :prepend f
    :clock-in t
    :clock-resume t)

   ("g" "Guitar Practice" entry
    (file+datetree ,(org-file "projects/music"))
    "* %U - Guitar Practice"
    :clock-in t
    :clock-resume t)

   ("j" "Japanese")
   ("ja" "Anki Japanese Reviews" entry
    (file+datetree ,(org-file "projects/languages/japanese"))
    "* %U - Anki Japanese Reviews :anki:"
    :clock-in t
    :clock-resume t)
   ("jb" "Bunpro" entry
    (file+datetree ,(org-file "projects/languages/japanese"))
    "* %U - Bunpro %^{Activity|Reviews|Lessons} :bunpro:"
    :clock-in t
    :clock-resume t)
   ("jw" "WaniKani" entry
    (file+datetree ,(org-file "projects/languages/japanese"))
    "* %U - WaniKani %^{Activity|Reviews|Lessons} :wanikani:"
    :clock-in t
    :clock-resume t)

   ("k" "Daily Journal" entry
    (file+datetree ,(org-file "review"))
    "* %U - %^{Activity}\n  %?")

   ("w" "Work")
   ,(my/agenda-item "w1" "1-on-1 Agenda Item"
                    (org-file "work")
                    "Manager 1-on-1 Meeting Agenda Items")
   ("wl" "Work Log" entry
    (file+olp+datetree ,(org-file "work_log"))
    "* %U - %^{Activity}\n  %?"
    :clock-in t
    :clock-resume t)
   ,(my/agenda-item "wi" "Issue To File"
                    (org-file "work")
                    "Issues to File")
   ("wm" "Work Meeting" entry
    (file+datetree ,(org-file "work_log"))
    "* %U - %^{Meeting} Meeting :meeting:\n  %?"
    :clock-in t
    :clock-resume t)
   ("wt" "Work Talk" entry
    (file+datetree ,(org-file "work_log"))
    "* %U - Talk with %^{With} about %^{About} :meeting:\n  %?"
    :clock-in t
    :clock-resume t)
   ("wr" "Code Review" entry
    (file+datetree ,(org-file "work_log"))
    "* %U - Code Review of %^{PR} :code_review:\n  %?"
    :clock-in t
    :clock-resume t)
   ("wp" "End of Day Work Review" entry
    (file+datetree ,(org-file "work_log"))
    "* %U - End of Day Review :review:\n  %?"
    :clock-in t
    :clock-resume t)
   ,(my/agenda-item "wr" "Team Retrospective Agenda Item"
                    (org-file "work")
                    "Team Retrospective Agenda Items")

   ("z" "Writing Idea" entry
    (file ,(org-file "writing"))
    "* TODO %^{Activity}\n  :PROPERTIES:\n  :Added: %U\n  :END:")))


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





;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(defun air-org-skip-subtree-if-not-habit ()
  "Skip an agenda entry if it has a STYLE property not equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (not (string= (org-entry-get nil "STYLE") "habit"))
        subtree-end
      nil)))









(provide 'init-org)
