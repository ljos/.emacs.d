(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(auctex
                      auto-complete
                      ace-jump-mode
                      ac-nrepl
                      browse-kill-ring
                      clojure-mode
                      clojurescript-mode
                      clojure-test-mode
                      coffee-mode
                      color-theme
                      color-theme-solarized
                      find-file-in-project
                      gist
                      jade-mode
                      langtool
                      nrepl
                      org
                      paredit
                      popwin
                      prolog
                      slime
                      slime-repl
                      starter-kit
                      starter-kit-bindings
                      starter-kit-lisp
                      sws-mode
                      undo-tree)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'elpa-config)
