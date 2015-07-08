(require 'ispell)

(when ispell-program-name
  ;; All programming language modes should derive from prog-mode;
  ;; flyspell-prog-mode activates spell-checking in comments and strings
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(provide 'init-spelling)
