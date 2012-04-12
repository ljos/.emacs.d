(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;;; Add in your own as you wish:
(defvar my-packages '(auto-complete
                      starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      slime
                      slime-repl
                      paredit
                      clojure-mode
                      clojure-test-mode
                      paredit
                      color-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
