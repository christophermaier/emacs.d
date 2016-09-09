(setq-default
 next-line-add-newlines t
 ;; line-number-mode t
 ;; column-number-mode t
 ;; size-indication-mode t
 require-final-newline t

 echo-keystrokes 0.1
 font-lock-maximum-decoration t
 visible-bell t)

;; recentf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default
 recentf-max-saved-items 200
 recentf-max-menu-items 20)

;; Ido
;; (mainly for org-switchb, since I'm using Helm pretty extensively)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ido-mode 1)
(setq ido-everywhere t
      ido-enable-flex-matching t)

;; Backups and Emacs spoor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default
 ;; Send backups and autosaves to the temp folder instead of polluting
 ;; the current directory.
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 create-lockfiles nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; Auto-revert buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-auto-revert-mode t)
(setq-default
 global-auto-revert-non-file-buffers t
 auto-revert-verbose t)

;; UTF-8 all the things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Tabs... evil, evil tabs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default
 indent-tabs-mode nil
 tab-width 4)

;; Text sizing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Also toss in OS X bindings for the hell of it
(defun restore-text-size ()
  "Remove all text scaling, returning it to the original size"
  (interactive)
  (text-scale-set 0))
(define-key global-map (kbd "s-=") 'text-scale-increase)
(define-key global-map (kbd "s--") 'text-scale-decrease)
(define-key global-map (kbd "s-0") 'restore-text-size)

;; Return key!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "C-j") 'newline-and-indent)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f5] 'whitespace-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'makefile-mode-hook
          (lambda () (whitespace-mode t)))

;; http://xahlee.org/emacs/whitespace-mode.html
(setq
 whitespace-display-mappings
 '((space-mark 32 [183] [46]) ;; normal space, ·
   (space-mark 160 [164] [95])
   (space-mark 2208 [2212] [95])
   (space-mark 2336 [2340] [95])
   (space-mark 3616 [3620] [95])
   (space-mark 3872 [3876] [95])
   (newline-mark 10 [182 10]) ;; newlne, ¶
   (tab-mark 9 [9655 9] [92 9]) ;; tab, ▷
   ))

;; Saveplace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saveplace"))
(setq-default save-place t)

;; Executable Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(provide 'init-editing)
