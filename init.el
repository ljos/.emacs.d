(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(if (and (window-system) (string= "darwin" system-type))
    (set-face-attribute 'default nil :font "menelo-14")
  (set-face-attribute 'default nil :font (font-get-system-font)))

(set-fringe-mode '(0 . 6))

(blink-cursor-mode -1)
(fringe-mode -1)
(global-hl-line-mode +1)
(global-linum-mode +1)
(scroll-bar-mode -1)
(show-paren-mode +1)
(tool-bar-mode -1)
(winner-mode +1)

(if (equal system-type 'gnu/linux)
    (menu-bar-mode -1)
  (menu-bar-mode +1))

;; The default is already set so it needs to be setq'ed instead. The
;; reason for setting this is that otherwise emacs starts at /.
(setq default-directory "~/")

(setq-default
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 color-theme-is-global t
 indent-tabs-mode nil
 inhibit-startup-message t
 inhibit-startup-screen t
 message-log-max t
 mouse-wheel-follow-mouse t ;; scroll window under mouse
 mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
 mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
 ns-pop-up-frames nil
 require-final-newline t
 ring-bell-function #'ignore
 use-dialog-box nil
 visible-bell nil
 )

(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(define-key 'help-command "a" 'apropos)

(global-set-key (kbd "s-[") 'backward-paragraph)
(global-set-key (kbd "s-]") 'forward-paragraph)
(global-set-key  [C-s-268632091] 'backward-sexp)
(global-set-key  [C-s-268632093] 'forward-sexp)

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defcustom exec-paths '("~/.lein/bin" "/usr/local/bin" "~/.local/bin" "/usr/texbin")
  "Directories to be added to exec-path"
  :type 'string)

(defun add-to-path (dir)
  "Adds a dir to PATH if dir exists."
  (when (file-exists-p dir)
    (progn (add-to-list 'exec-path dir)
           (setenv "PATH" (concat (getenv "PATH") (concat ":" dir))))))

(defun initialize-exec-path ()
  (interactive)
  (dolist (dir exec-paths)
    (add-to-path dir)))

(initialize-exec-path)

;; found at http://www.emacswiki.org/emacs/SortWords
(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

;; Convenience function to get all keys in a hash table.
(defun keys (hashtable)
  "Return all keys in hashtable."
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys))) hashtable)
    allkeys))

;;; Found at http://stackoverflow.com/a/94277
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    (add-to-list 'default-frame-alist
                 (cons 'width
                       (if (> (x-display-pixel-width) 1280)
                           100 80)))
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 50)
                          (frame-char-height)))))))

(defun x-maximize-frame ()
    (interactive)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(if (eq 'x window-system)
    (x-maximize-frame)
  (set-frame-size-according-to-resolution))

(set-frame-position (next-frame) 0 0)

(setq mode-line-in-non-selected-windows nil)

(defun truncate-string-to-length (str end-column &optional start-column padding ellipsis)
  "The same as truncate-string-to-width,
except it truncates from the start of the list"
  (concat
   (reverse
    (append (truncate-string-to-width
             (concat (reverse (append (format  str) nil)))
             end-column start-column padding ellipsis)
            nil))))

(make-face 'mode-line-minor-mode-face)

(set-face-attribute 'mode-line nil
  :box '(:line-width 1
         :color "gray25"))

(set-face-attribute 'mode-line-minor-mode-face nil
  :inherit 'mode-line-face
  :height 110)

(defun center-string-in-char (str len char)
  (store-substring (make-string len char)
                   (/ (- len (length str)) 2) str))

(setq-default mode-line-position
              '(" %03l:%2c"))

;;; Modeline configs
(setq-default pomodoro-mode-line-string "")
(setq-default mode-line-format
  '("%e "
    (:eval (if buffer-file-name "%* " "無常"))        ; file status
    (:eval
     (propertize
      (if (buffer-narrowed-p)
          " 狭"
        "")))

    mode-line-position
    "  "
    (:eval
     (propertize                        ; file/buffer name
      (center-string-in-char
       (truncate-string-to-length
        (or buffer-file-truename
            (buffer-name))
        25 nil nil  "..")
       25 ?\s)
      'help-echo (buffer-file-name)     ; echo full name
      'local-map
      (let ((map (make-sparse-keymap)))
        (define-key map [mode-line mouse-3]
          'mode-line-next-buffer)
        (define-key map [mode-line mouse-1]
          'mode-line-previous-buffer)
        map)))

    "  "

    (:eval
     (propertize mode-name
                 'help-echo (format-mode-line minor-mode-alist)))
    " "
    vc-mode
    "  "

    pomodoro-mode-line-string

    (:eval
     (concat
      (propertize " " 'display
                 `((space :align-to
                           (- right ,(if (string= "" pomodoro-mode-line-string) 20 8)))))
      (propertize (if (string= "" pomodoro-mode-line-string)
                      (format-time-string " %a %b %d, %H:%M")
                    (format-time-string " %H:%M"))                 ; time
                  'help-echo
                  (format-time-string "%A, %B %d, %Y, %H:%M"))))))


(defun conditionally-enable-paredit-mode ()
  "enable paredit-mode during eval-expression"
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

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
    arduino-mode
    auctex
    auto-complete
    browse-kill-ring
    cider
    cljdoc
    clojure-mode
    clojure-test-mode
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
    midje-mode
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
    use-package
    undo-tree
    visual-regexp
    ;; yasnippet
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'use-package)

(use-package ace-jump-mode
  :bind ("C-x SPC" . ace-jump-mode))

(use-package arduino-mode
  :mode ("\\.ino$" . arduino-mode)
  :config (add-hook 'arduino-mode-hook '(lambda ()
                                          (idle-highlight-mode +1))))

(use-package auto-complete-config
  :init (ac-config-default)
  :config
  (progn
    (setq ac-auto-show-menu 0.3)
    (setq ac-use-menu-map t)
    (ac-config-default)
    (setq ac-sources
          (cons ac-source-yasnippet
                ac-sources))
    (define-key ac-complete-mode-map "\r" 'ac-expand)
    (define-key ac-complete-mode-map [return] 'ac-expand)
    (define-key ac-complete-mode-map "\t" 'ac-complete)
    (define-key ac-complete-mode-map [tab] 'ac-complete)
    (global-auto-complete-mode)))

(use-package browse-kill-ring
  :init (browse-kill-ring-default-keybindings))

(use-package paredit
  :commands (enable-paredit-mode paredit-mode)
  :config
  (progn
    (defun paredit-delete-indentation ()
      (interactive)
      (delete-indentation)
      (prog-indent-sexp))

    (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
    (define-key paredit-mode-map (kbd "M-)") 'paredit-close-round-and-newline)
    (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
    (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
    (define-key paredit-mode-map (kbd "M-}") 'paredit-close-curly-and-newline)
    (define-key paredit-mode-map (kbd "M-j") 'paredit-delete-indentation)))

(use-package cider
  :config
  (progn
    (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
    (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
    (add-hook 'cider-repl-mode-hook 'subword-mode)
    (setq cider-repl-history-file "~/.emacs.d/history/nrepl")))

(use-package yasnippet
  :commands (yas-global-mode yas-activate-extra-mode)
  :load-path "site-lisp/yasnippet"
  :init (yas-global-mode +1))


(use-package clojure-mode
  :mode (("\\.cljx?$" . clojure-mode)
         ("\\.dtm$" . clojure-mode)
         ("\\.edn$" . clojure-mode))
  :interpreter (("jark" . clojure-mode)
                ("cake" . clojure-mode))
  :config
  (progn
    (use-package cljdoc
      :config
      (progn
        (defadvice cljdoc-get-docstring (after truncate-docstring)
          (setq ad-return-value
                (truncate-string-to-width
                 (concat " " ad-return-value) (- (frame-width) 10) nil nil 't)))
        (ad-activate 'cljdoc-get-docstring)))
    (use-package clojure-jump-to-file)
    (use-package midje-mode
      :config (add-hook 'midje-mode-hook '(lambda ()
                                            (yas-activate-extra-mode 'midje-mode))))

    (defun clojure-jump-to-project-file ()
      (interactive)
      (let ((dir (file-name-as-directory
                  (locate-dominating-file buffer-file-name "src/"))))
        (find-file (concat dir "project.clj"))))

    (defvar clojure-mode-initialized nil)

    (defun my-clojure-mode-hook ()
      (unless clojure-mode-initialized
        (define-key clojure-mode-map  (kbd "C-x p") 'clojure-jump-to-project-file)

        (put-clojure-indent 'update-in 'defun)
        (put-clojure-indent 'get-in 'defun)
        (put-clojure-indent 'assoc-in 'defun)
        (put-clojure-indent 'assoc! 'defun)
        (put-clojure-indent 'swap! 'defun)
        (put-clojure-indent 'run* 'defun)
        (put-clojure-indent 'fresh 'defun)

        (setq clojure-mode-initialized t))
      (enable-paredit-mode))

    (add-hook 'clojure-mode-hook 'my-clojure-mode-hook)))

(define-derived-mode clojurescript-mode clojure-mode "ClojureScript"
  "Major mode for ClojureScript")

(use-package clojurescript-mode
  :mode ("\\.cljs$" . clojurescript-mode))

(use-package command-frequency
  :init (command-frequency-mode +1))

(use-package dired
  :init
  (if  (not (boundp 'dired-mode-map))
      (add-hook 'dired-load-hook
                (lambda ()
                  (define-key dired-mode-map [return]
                    'dired-single-buffer)
                  (define-key dired-mode-map [mouse-1]
                    'dired-single-buffer-mouse)
                  (define-key dired-mode-map "^"
                    (function
                     (lambda ()
                       (interactive)
                       (dired-single-buffer "..")))))))
  :config
  (use-package dired-single))



(use-package lisp-mode
  :config
  (progn
    (use-package elisp-slime-nav
      :commands elisp-slime-nav-mode)

    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (make-local-variable 'after-save-hook)
                (add-hook 'after-save-hook
                          (lambda ()
                            (if (file-exists-p (concat buffer-file-name "c"))
                                (delete-file (concat buffer-file-name "c")))))))

    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package color-theme-sanityinc-tomorrow
  :if window-system
  :config
  (progn
    (use-package color-theme)
    (setq custom-enabled-themes '(sanityinc-tomorrow-night))
    (setq custom-safe-themes '("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a"
                               default))
    (color-theme-sanityinc-tomorrow-night)
    (set-face-attribute 'linum nil :foreground "#969896")))

(use-package ffap
  :init (ffap-bindings))

(use-package go-mode
  :mode ("\\.go$" . go-mode))

(use-package haskell-mode
  :mode (("\\.hs$" . haskell-mode)
         ("\\.lhs$" . literate-haskell-mode))
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (use-package ibuffer-vc
      :commands ibuffer-vc-set-filter-groups-by-vc-root)
    (use-package ibuffer-ext
      :commands ibuffer-do-sort-by-major-mode)

    (defvar ibuffer-initialized nil)
    (defun my-ibuffer-hook ()
      (unless ibuffer-initialized
        (ibuffer-vc-set-filter-groups-by-vc-root)

        (unless (eq ibuffer-sorting-mode 'major-mode)
          (ibuffer-do-sort-by-major-mode))

        (setq ibuffer-formats
              '((mark modified read-only vc-status-mini " "
                      (name 25 25 :left :elide)
                      " "
                      (size 9 -1 :right)
                      " "
                      (mode 16 16 :left :elide)
                      " "
                      (vc-status 16 16 :left)
                      " "
                      filename-and-process)))
        (setq ibuffer-expert t)
        (setq ibuffer-initialized t)))
    (add-hook 'ibuffer-hook 'my-ibuffer-hook)))

(use-package ido
  :init (ido-mode +1)
  :config
  (progn
    (use-package flx-ido
      :commands flx-ido-mode)
    (use-package ido-vertical-mode
      :commands ido-vertical-mode)
    (flx-ido-mode +1)
    (ido-vertical-mode +1)
    (setq id-use-faces nil
          ido-auto-merge-work-directories-length nil
          ido-create-new-buffer 'always
          ido-enable-flex-matching t
          ido-enable-prefix nil
          ido-handle-duplicate-virtual-buffers 2
          ido-max-prospects 10
          ido-use-filename-at-point 'guess
          ido-use-virtual-buffers t)))

(use-package ielm
  :defer t
  :config
  (add-hook ielm-mode-hook 'enable-paredit-mode))

(use-package ispell
  :defer t
  :config
  (setq-default ispell-program-name "/usr/local/bin/aspell"))

(use-package linum
  :init (global-linum-mode +1)
  :config
  (progn
    (defvar my-linum-format-string "%4d")

    (add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

    (defun my-linum-get-format-string ()
      (let* ((width (length (number-to-string
                             (count-lines (point-min) (point-max)))))
             (format (concat "%" (number-to-string width) "d ")))
        (setq my-linum-format-string format)))

    (defun my-linum-format (line-number)
      (propertize (format my-linum-format-string line-number) 'face 'linum))

    (setq linum-format 'my-linum-format)))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package org-mode
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cb" . org-iswitchb)
         ("\C-cc" . org-capture))
  :config
  (progn
    (defvar org-mode-initialized nil)
    (defun my-org-mode-hook ()
      (unless org-mode-initialized
        (setq org-directory "~/Dropbox/org"
              org-mobile-inbox-for-pull "~/Dropbox/org/inbox.org"
              org-mobile-directory "~/Dropbox/org/mobile"

              org-agenda-include-all-todo t
              org-agenda-files '("~/Dropbox/org/organizer.org")

              org-tag-persistent-alist '(("work" . ?w) ("private" . ?p))

              org-todo-keywords '((sequence "TODO" "STARTED" "WAITING"
                                            "|" "DONE" "CANCELLED" "ON-HOLD" "DEFERRED" "DELEGATED")
                                  (sequence "APPT" "|" "FINISHED" "CANCELLED" "MISSED")
                                  (sequence "BUG" "|" "FIXED")
                                  (sequence "NOTE"))

              org-todo-keyword-faces '(("STARTED" . "yellow")
                                       ("ON-HOLD" . "orange")
                                       ("CANCELLED" . "dim gray")
                                       ("NOTE" . "aqua"))

              org-refile-targets '(("organizer.org" :maxlevel . 9))
              org-completion-use-ido t
              org-latex-to-pdf-process '("texi2dvi --pdf --verbose --batch %f"))

       (unless (boundp 'org-export-latex-classes)
         (setq org-export-latex-classes nil))

       (add-to-list 'org-export-latex-classes
                    '("article"
                      "\\documentclass{article}
                \\usepackage[l2tabu, orthodox]{nag}
                \\usepackage{microtype}"
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

       (add-to-list 'org-export-latex-classes
                    '("thesis"
                      "\\documentclass{report}
                \\usepackage[l2tabu, orthodox]{nag}
                \\usepackage{microtype}"
                      ("\\chapter{%s}" . "\\chapter*{%s}")
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

       (define-key org-mode-map (kbd "M-q") 'org-fill-paragraph)
       (visual-line-mode t)
       (setq fill-column 80)
       (setq ispell-parser 'tex)
       (font-lock-remove-keywords
        nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)\\b"
               1 font-lock-warning-face t)))
       (add-to-list 'org-latex-packages-alist '("" "microtype"))
       (org-add-link-type
        "citet*" 'ebib
        (lambda (path desc format)
          (cond
           ((eq format 'latex)
            (if (or (not desc) (equal 0 (search "citet*:" desc)))
                (format "\\citet*{%s}" path)
              (format "\\citet*[%s]{%s}" desc path))))))


       ;;org-capture config
       (setq org-default-notes-file (concat org-directory "/organizer.org"))

       (setq org-capture-templates
             '(("a" "Appointments" entry
                (file+headline org-default-notes-file "Appointments")
                "* APPT %? %^{WITH}p %^{LOCATION}p\n%^T--%^T\n" :prepend)
               ("p" "Project" entry
                (file+headline org-default-notes-file "Projects")
                "* %?\n")
               ("d" "Done" entry
                (file+datetree (concat org-directory "/done.org"))
                "* %?\nCLOCK: %^U--%U")
               ("j" "Journal" entry
                (file+datetree (concat org-directory "/journal.org"))
                "* %?\nEntered on %U\n  %i\n  %a")
               ("n" "Note" entry
                (file+headline org-default-notes-file "Notes")
                "* NOTE %?\n")
               ("t" "Todo" entry
                (file+headline org-default-notes-file "Tasks")
                "* TODO %?\n  %i\n")))

       (defun org-export-latex-no-toc (depth)
         (when depth
           (format "%% Org-mode is exporting headings to %s levels.\n"
                   depth)))
       (setq org-export-latex-format-toc-function 'org-export-latex-no-toc)
       (setq org-mode-initialized t)))))

(use-package pomodoro
  :commands pomodoro-start
  :bind (("C-c p s" . pomodoro-start)
         ("C-c p x" . pomodoro-stop))
  :config
  (progn
    (setq pomodoro-break-start-sound "~/Music/smw_pause.wav"
          pomodoro-work-start-sound "~/Music/smw_pause.wav"
          pomodoro-work-start-message "Back to work!"
          pomodoro-work-cycle "労働" ;; work in japanese
          pomodoro-break-cycle "休止" ;; break in japanese
          pomodoro-long-break-time 20
          pomodoro-break-time 7)))

(use-package popwin
  :commands popwin:display-buffer
  :init (setq display-buffer-function 'popwin:display-buffer))

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook (lambda () (idle-highlight-mode +1))))

(use-package prolog-mode
  :commands (run-prolog prolog-mode mercury-mode)
  :mode (("\\.pl$" . prolog-mode)
         ("\\.m$" . mercury-mode))
  :config
  (setq prolog-system 'swi))

(use-package python-mode
  :commands python-mode
  :mode ("\\.py$" . python-mode)
  :config
  (progn
    (use-package python-pep8)
    (use-package python-pylint)))

(use-package saveplace
  :init
  (setq-default save-place-file (concat user-emacs-directory "places")
                save-place t))

(setq inferior-lisp-program "sbcl")
(load-file (expand-file-name "~/quicklisp/slime-helper.el"))
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex)
  :config
  (progn
    (setq smex-save-file (concat user-emacs-directory ".smex-items"))))

(use-package sparql-mode
  :load-path "site-lisp/sparql-mode"
  :mode ("\\.sparql$" . sparql-mode)
  :config
  (progn
    (add-to-list 'ac-dictionary-files "~/.emacs.d/site-lisp/sparql-mode/sparql-mode")
    (setq sparql-default-base-url "http://live.dbpedia.org/sparql")
    (add-hook 'sparql-result-mode-hook '(lambda () (linum-mode -1)))
    (add-hook 'sparql-result-mode-hook '(lambda () (toggle-truncate-lines 1)))))

(use-package undo-tree
  :init (global-undo-tree-mode))

(use-package visual-regexp
  :commands (vr/replace vr/query-replace)
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))
