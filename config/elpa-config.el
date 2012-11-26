(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(
    ac-nrepl
    ace-jump-mode
    auctex
    auto-complete
    browse-kill-ring
    clojure-mode
    clojure-test-mode
    clojurescript-mode
    coffee-mode
    color-theme
    color-theme-solarized
    expand-region
    find-file-in-project
    gist
    jade-mode
    js2-mode
    langtool
    nrepl
    org
    paredit
    popwin
    pomodoro
    prolog
    python-mode
    python-pep8
    python-pylint
    slime
    slime-repl
    starter-kit
    starter-kit-bindings
    starter-kit-lisp
    sws-mode
    undo-tree
    yasnippet
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'elpa-config)
