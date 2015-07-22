;; Adapted from a reply to an org-mode mailing list post of mine from
;; 4 years ago (which I forgot I had even posted!)
;;
;; https://lists.gnu.org/archive/html/emacs-orgmode/2010-10/msg01625.html
;;
;; Intended to be used as an org-mode sexp recurring date entry
;; (e.g., DEADLINE: <%%(cwmaier/work-payday date)>)
(defun cwmaier/work-payday (date)
  "Returns TRUE if DATE is one of my bi-monthly paydays: the
  15th (or nearest preceeding workday) and the last workday of the
  month. This doesn't take into account company holidays"
  (let* ((dayname (calendar-day-of-week date))
         (is-weekday (memq dayname '(1 2 3 4 5)))
         (is-friday (= dayname 5))
         (day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (lastday (calendar-last-day-of-month month year))
         (is-last-day (= day lastday))
         (last-two-days-before-last-day (list (- lastday 2) (- lastday 1))))
    (or (and (= day 15) is-weekday)
        (and (memq day '(13 14)) is-friday)
        (and is-last-day is-weekday)
        (and (memq day last-two-days-before-last-day) is-friday))))

(defun cwmaier/format-date (date)
  "DATE is (MONTH DAY YEAR)"
  (format "%d-%02d-%02d"
          (calendar-extract-year date)
          (calendar-extract-month date)
          (calendar-extract-day date)))

(defun cwmaier/days-from-date (date offset)
  "Returns (MONTH DAY YEAR) of OFFSET days from DATE"
  (calendar-gregorian-from-absolute
   (+ offset
      (calendar-absolute-from-gregorian
       date))))

(defun cwmaier/this-coming-day-date (coming-day)
  "Return the date of this coming COMING-DAY (or today, if today
  is COMING-DAY). COMING-DAY is an integer, with 0 = Sunday, 1 =
  Monday, etc."
  (let* ((today (calendar-current-date))
         (day-of-week (calendar-day-of-week today))
         (diff (- day-of-week coming-day))
         (to-add (if (< diff 0)
                     (abs diff)
                   (- 7 diff)))
         (coming-date (calendar-current-date to-add)))
    coming-date))

(defun cwmaier/this-weekends-saturday ()
  "Return (MONTH DAY YEAR) for this coming Saturday. If today is
  Saturday, return today's date. If today is Sunday, return
  yesterday's date"
  (let* ((today (calendar-current-date))
         (day-of-week (calendar-day-of-week today)))
    (cond
     ;; today is saturday
     ((= 6 day-of-week) today)
     ;; today is sunday; yesterday was the start of this weekend
     ((= 0 day-of-week) (calendar-current-date -1))
     ;; must be next weekend, then
     ((cwmaier/this-coming-day-date 6)))))

(defun cwmaier/this-weekends-sunday ()
  "Returns (MONTH DAY YEAR) of the day after
  `cwmaier/this-weekends-saturday`"
  (cwmaier/days-from-date (cwmaier/this-weekends-saturday) 1))

(defun cwmaier/one-week-from-today ()
  "Returns (MONTH DAY YEAR) of one week from now"
  (calendar-current-date 7))

(defun cwmaier/org-date (date)
  "DATE is (MONTH DAY YEAR). Returns formatted string for
  embedding in an Org date, e.g. '2015-07-13 Mon'"
  (concat (cwmaier/format-date date)
          " "
          (calendar-day-name date t)))

(defun my/agenda-item (key description file heading)
  "Helpful function to create entries for `org-capture-templates` for agenda items for meeting with people"
  `(,key ,description entry (file+headline ,file ,heading) "* TODO %^{Item}\n  %U\n  %?" :prepend t))

(defun my/direct-report-item (key description file)
  "Create entries for `org-capture-templates` for capturing info about team members"
  `(,key ,description entry (file+datetree ,file) "** %U - %^{Activity}\n   %?"))

(provide 'init-utils)
