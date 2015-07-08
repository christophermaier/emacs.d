;; Adapted from a reply to an org-mode mailing list post of mine from
;; 4 years ago (which I forgot I had even posted!)
;;
;; https://lists.gnu.org/archive/html/emacs-orgmode/2010-10/msg01625.html
;;
;; Intended to be used as an org-mode sexp recurring date entry
;; (e.g., =DEADLINE: <%%(cwmaier/work-payday date>=)
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

(provide 'init-utils)
