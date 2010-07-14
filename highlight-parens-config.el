;; Stolen from http://stackoverflow.com/questions/2413047/how-do-i-get-rainbow-parentheses-in-emacs/2413472#2413472
(setq hl-paren-colors
      '(;"#8f8f8f" ; this comes from Zenburn
                   ; and I guess I'll try to make the far-outer parens look like this
        "orange1" "yellow1" "greenyellow" "green1"
        "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))

(provide 'highlight-parens-config)
