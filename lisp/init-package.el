(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize) ;; Try (package-initialize t) to not autoload packages

;; Have to have a list of packages to operate from!
(package-refresh-contents)

;; until I get gpg sorted
(setq package-check-signature nil)

(defun require-package
    (package)
  "Installs PACKAGE. Might wrap some other functionality around
the bare `package-install`, so that's why this function exists."
  (progn
    (package-install package)
    (require package)))

(provide 'init-package)
