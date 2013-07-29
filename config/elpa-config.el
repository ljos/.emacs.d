(require 'package)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

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
    cljdoc
    clojure-mode
    clojure-test-mode
    clojurescript-mode
    coffee-mode
    command-frequency
    color-theme
    color-theme-sanityinc-tomorrow
    dired-single
    expand-region
    find-file-in-project
    evil
    evil-paredit
    gist
    go-mode
    ibuffer-vc
    ido-ubiquitous
    jade-mode
    js2-mode
    langtool
    magit
    magithub
    midje-mode
    multi-web-mode
    multiple-cursors
    nrepl
    org
    paredit
    php+-mode
    popwin
    pomodoro
    prolog
    python-mode
    python-pep8
    python-pylint
    skewer-mode
    slime
    slime-repl
    smex
    sws-mode
    undo-tree
    workgroups
    yasnippet
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'elpa-config)
