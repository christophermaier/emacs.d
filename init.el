(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

;; (set-frame-font "Source Code Pro-16" nil t)

(require 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up package archives and prepare use-package; everything else
;; builds on that.
(require 'package)
(setq package-enable-at-startup nil)
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
 (require 'use-package))
(setq use-package-always-ensure t)
(use-package diminish) ;; if using :diminish
(require 'bind-key) ;; if using any :bind variant

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package
  :custom
  (system-packages-package-manager 'nix)
  (system-packages-use-sudo nil))

(use-package nix-mode
  :mode "\\.nix\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; todo remove this soon
(require 'init-package)

(use-package hc-zenburn-theme
  :config
  (load-theme 'hc-zenburn t))

(use-package direnv
  :ensure-system-package direnv
  :config
  (direnv-mode))

(use-package flycheck
  ;; pull from melpa in order to get a version that's got support for `go vet`
  ;; See https://github.com/flycheck/flycheck/pull/1548
  :pin melpa
  :config
  (global-flycheck-mode)
  ;; Really tired of seeing all the Emacs Lisp documentation checks
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc)))
;; TODO Get better bindings for navigating errors
;; Have error window pop up automatically

(use-package company
  :config (global-company-mode)
  :diminish company-mode
  :custom
  (company-idle-delay 0.2)
  (company-tooltip-align-annotations 5)
  :bind (:map company-active-map
         ;; Helm has spoiled me; can't use M-n / M-p anymore :)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)))

(use-package fullframe)


(require 'init-frames)

(require 'init-editing)

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ;; Normally `C-x m` is bound to `compose-mail`, but I never use that
         ;; `C-c m` isn't bound to anything; I'll set both here and see which
         ;; I like best :P
         ("C-x m" . helm-bookmarks)
         ("C-c m" . helm-bookmarks)
         :map helm-map
         ;; This makes helm completion behave a bit more like normal
         ;; Emacs completion, particularly in the context of
         ;; helm-find-files
         ("<tab>" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)))
(use-package helm-swoop
  ;; Yup, overriding isearch-forward
  :bind ("C-s" . helm-swoop)
  :after (helm))
(use-package helm-ls-git
  ;; Another fun thing is =C-]=, which toggles full paths on and off.
  :bind ("C-x C-d" . helm-browse-project)
  :after (helm))
(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds)
  :after (helm))

(use-package helm-flycheck
  :bind (:map flycheck-mode-map
              ;; (Though it's not toooooo terribly different from just running
              ;;   C-c ! l (flycheck-list-errors)
              ;;
              ;; Overall consistency is good, though, I suppose
              ("C-c ! h" . helm-flycheck))
  :after (helm flycheck))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom (projectile-use-git-grep t)
  :config (projectile-global-mode))
(use-package helm-projectile
  :after (projectile helm))

;; Some key commands:
;; C-c p p -> switch project
;; C-c p h -> helm!
;; C-c p t -> toggle with test file (doesn't seem to go back to impl, though)
;; C-c p k -> kill current project buffers
;; C-c p S -> save current project buffers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :bind ([f7] . magit-status)
  :config
  (fullframe magit-status magit-mode-quit-window)
  :custom
  (magit-commit-arguments (quote ("--signoff"))))
(use-package github-browse-file
  :bind ([f8] . github-browse-file))
(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :config (global-git-gutter-mode t)
  :custom (git-gutter-fr:side 'left-fringe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-prog-mode)
;; (require 'init-ruby)

(use-package paredit
  :diminish "()")
(use-package paredit-everywhere
  :diminish "()-"
  :hook (prog-mode . paredit-everywhere-mode)
  :after (paredit))

(require 'init-lisp)
;; (require 'init-clojure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t)
  :bind
  (:map rust-mode-map
        ([tab] . company-indent-or-complete-common)))
(use-package flycheck-rust
  :after (rust-mode flycheck)
  :hook (flycheck-mode . flycheck-rust-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package erlang
  :mode "\\.erl\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package alchemist
  :mode "\\.ex\\'"
  :custom (alchemist-hooks-compile-on-save t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :custom
  (gofmt-command "goimports")
  :hook (before-save . gofmt-before-save))
(use-package company-go
  :after (go-mode company))
(use-package go-projectile
  ;; Not sure this gets me a whole bunch...
  :after (go-mode))
(use-package flycheck-golangci-lint
  ;; not sure about this one either
  ;; brew install golangci/tap/golangci-lint
  ;; brew upgrade golangci/tap/golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-mode
  :mode "\\.hs\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/snippet-mode-hook ()
  "Don't add newlines to the end of snippets, since that cause
them to get inserted in the snippet's expansion."
  (setq-local require-final-newline nil))

(use-package yasnippet
  :mode "\\.yasnippet\\'"
  :diminish yas-minor-mode
  :config (yas-global-mode 1)
  :hook (snippet-mode . my/snippet-mode-hook))

;; https://emacs.stackexchange.com/a/7909
(defun aya-open-line ()
  "Call `open-line', unless there are abbrevs or snippets at point.
In that case expand them.  If there's a snippet expansion in progress,
move to the next field. Call `open-line' if nothing else applies."
  (interactive)
  (cond ((expand-abbrev))

        ((yas--snippets-at-point)
         (yas-next-field-or-maybe-expand))

        ((ignore-errors
           (yas-expand)))

        (t
         (open-line 1))))
(global-set-key "\C-o" 'aya-open-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode)

(use-package terraform-mode)

(use-package graphviz-dot-mode
  :ensure-system-package (dot . graphviz))

;; (require 'init-spelling)
;; (require 'init-eshell)
(require 'init-org)
(use-package helm-org
  :after (helm))
;; TODO Look into various org add-ons

(use-package typescript-mode
  :mode "\\.ts\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/emacs-lsp/lsp-mode/issues/517
;; TL;DR - If things don't work, delete old versions of Dash

(use-package lsp-mode
  :pin melpa-stable
  :hook ((go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (sh-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :custom (lsp-prefer-flymake nil))
(use-package lsp-ui
  :pin melpa-stable
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; where did this go?
;; (use-package lsp-ui-flycheck
;;   :after (lsp flymake)
;;   :custom (lsp-ui-flycheck-enable t))

;; Bash LSP Server
;; (need to get npm set up with asdf)
;; npm i -g bash-language-server

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package gnuplot
  :ensure-system-package gnuplot)

(use-package powershell
  :mode ("\\.ps1\\'" . powershell-mode)
  :ensure-system-package (pwsh . powershell))

(use-package dockerfile-mode)
(use-package docker-compose-mode)

;; TODO Create a hydra for this; there are a lot of commands
(use-package markdown-mode
  :ensure-system-package multimarkdown
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook (markdown-mode . auto-fill-mode))

(use-package bats-mode
  :mode "\\.bats\\'")

(use-package origami
  :config
  (global-origami-mode)
  (defhydra folding-with-origami-mode (global-map "C-c f")
  ("h" origami-close-node-recursively "Hide")
  ("o" origami-open-node-recursively  "Open")
  ("t" origami-toggle-all-nodes  "Toggle buffer")
  ("n" origami-next-fold "Next")
  ("p" origami-previous-fold "Previous"))
  :after (hydra))
;; (use-package lsp-origami
;;   :after (lsp-mode origami)
;;   :hook (origami-mode . lsp-origami-mode))

(use-package geiser
  :hook (geiser-repl-mode . paredit-mode)
  :custom
  (geiser-active-implementations '(racket)) ;; for now, at least
  (geiser-racket-binary "/Applications/Racket v7.4/bin/racket")
  :after (paredit))

(use-package systemd
  :mode "\\.service\\'")

;; Stolen from Steve Purcell
(global-set-key (kbd "C-h K") 'find-function-on-key)

(require 'server)
(unless (server-running-p)
  (server-start))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-org nix-mode processing-mode typescript-mode js2-mode use-package-ensure-system-package systemd use-package terraform-mode rust-mode ruby-tools rainbow-delimiters protobuf-mode powershell paredit-everywhere origami multiple-cursors magit lsp-ui hydra hl-sexp highlight-symbol helm-swoop helm-projectile helm-ls-git helm-flycheck helm-descbinds helm-c-yasnippet hc-zenburn-theme haskell-mode graphviz-dot-mode go-projectile gnuplot github-browse-file git-gutter-fringe geiser fullframe flycheck-rust flycheck-golangci-lint exec-path-from-shell erlang dockerfile-mode docker-compose-mode direnv diminish company-go column-marker cider bats-mode alchemist))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
