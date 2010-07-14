;; Apparently highlight-parentheses-mode doesn't provide a way to programmatically activate it
;; (you need to do it manually with `M-x highlight-parentheses-mode`)
;; This is a pain, so we'll provide such a way, and go ahead and activate it globally.
;;
;; Stolen from http://nflath.com/2010/02/emacs-minor-modes-mic-paren-pager-dired-isearch-whichfunc-winpoint-and-highlight-parentheses/
(defun turn-on-highlight-parentheses-mode ()
  (highlight-parentheses-mode t))
(define-global-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  turn-on-highlight-parentheses-mode)

(global-highlight-parentheses-mode)

;; Since the default colors for highlight-parentheses-mode are kind of terrible, and I'd prefer
;; "rainbow parens", we'll override the colors
;;
;; Stolen from http://stackoverflow.com/questions/2413047/how-do-i-get-rainbow-parentheses-in-emacs/2413472#2413472
(setq hl-paren-colors
      '(;"#8f8f8f" ; this comes from Zenburn
                   ; and I guess I'll try to make the far-outer parens look like this
        "orange1" "yellow1" "greenyellow" "green1"
        "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))

(provide 'highlight-parens-config)
