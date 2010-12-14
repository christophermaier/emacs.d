;; Stolen from Steve Purcell
;; `https://github.com/purcell/emacs.d/blob/master/init-slime.el'

(eval-after-load "slime"
  '(progn
     (require 'ac-slime)
     (add-hook 'slime-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
 
     (eval-after-load "auto-complete"
       '(add-to-list 'ac-modes 'slime-repl-mode))))

(provide 'slime-config)
