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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; todo remove this soon
(require 'init-package)

(use-package hc-zenburn-theme)

;; Consider
;; https://github.com/jwiegley/use-package/tree/1d5ffb2e0d1427066ced58febbba68c1328bf001#use-package-ensure-system-package
;; to install direnv if it's not there already
(use-package direnv
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

(require 'init-exec-path)
(require 'init-frames)
(require 'init-windows)
(require 'init-editing)

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
         ("[tab]" . helm-execute-persistent-action)
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
  :custom (projectile-use-git-grep t)
  :config (projectile-global-mode))
(use-package helm-projectile
  :after (projectile helm))

;; Unsure why I need these now?
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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

(require 'init-yasnippet)

(use-package yaml-mode)
(use-package terraform-mode)
(use-package graphviz-dot-mode)

;; (require 'init-spelling)
;; (require 'init-eshell)
(require 'init-org)
(use-package helm-org
  :after (helm))
;; TODO Look into various org add-ons

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
  :hook (lsp-mode . lsp-ui-mode))

;; where did this go?
;; (use-package lsp-ui-flycheck
;;   :after (lsp flymake)
;;   :custom (lsp-ui-flycheck-enable t))

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
  :if (executable-find "gnuplot"))

(use-package powershell
  :mode ("\\.ps1\\'" . powershell-mode))

(use-package dockerfile-mode)
(use-package docker-compose-mode)

;; TODO ensure markdown engine is present on PATH
;; TODO Create a hydra for this; there are a lot of commands
(use-package markdown-mode
  :mode "\\.md\\'")

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
    (powershell lsp-origami origami lsp-ui-flycheck hydra flycheck-golangci-lint go-projectile company-go rust-mode direnv-mode use-package haskell-process haskell-interactive-mode tide flycheck-color-mode-line protobuf-mode toml-mode gnuplot terraform-mode helm-c-yasnippet yasnippet cider clojure-mode hl-sexp paredit-everywhere paredit ruby-tools ruby-mode highlight-symbol column-marker rainbow-delimiters git-gutter-fringe github-browse-file helm-projectile helm-descbinds helm-swoop helm-ls-git helm exec-path-from-shell fullframe diminish))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
