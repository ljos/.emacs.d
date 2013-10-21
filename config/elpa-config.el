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
    ace-jump-mode
    auctex
    auto-complete
    browse-kill-ring
    cider
    cljdoc
    clojure-mode
    color-theme
    color-theme-sanityinc-tomorrow
    command-frequency
    dired-single
    elisp-slime-nav
    expand-region
    find-file-in-project
    go-mode
    ibuffer-vc
    idle-highlight-mode
    ido-ubiquitous
    magit
    org
    paredit
    pomodoro
    popwin
    prolog
    python-mode
    python-pep8
    python-pylint
    slime
    slime-repl
    smex
    undo-tree
    visual-regexp
    yasnippet
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'elpa-config)
