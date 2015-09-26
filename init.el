;;; init.el -- Initializes emacs
(require 'package)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

;; (setq use-package-verbose t)

(eval-when-compile
  (require 'use-package))

(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (with-temp-buffer
    (write-file custom-file)))
(load custom-file)

(when window-system
  (set-face-attribute 'default nil :font "fira-mono-14")
  (add-to-list 'default-frame-alist `(width . 100))
  (add-to-list 'default-frame-alist `(height . ,(/ (- (display-pixel-height) 50)
                                                   (frame-char-height)))))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :commands color-theme-sanityinc-tomorrow-night
  :init
  (unless (member 'sanityinc-tomorrow-night custom-enabled-themes)
    (color-theme-sanityinc-tomorrow-night)))

(defun ljos/back-to-indentation|beginning-of-line ()
  "Moves the cursor back to indentation or to the beginning of
the line if it is already at the indentation.  If it is at the
beginning of the line it stays there."
  (interactive)
  (when (not (bolp))
    (let ((p (point)))
      (back-to-indentation)
      (when (= p (point))
        (beginning-of-line 1)))))


(require 'bind-key)
(bind-keys*
 ("s-[" . backward-paragraph)
 ("s-]" . forward-paragraph)
 ("C-s" . isearch-forward-regexp)
 ("C-r" . isearch-backward-regexp)
 ("C-a" . ljos/back-to-indentation|beginning-of-line))

(set-locale-environment "utf-8")
(setenv "LANG" "en_US.UTF-8")

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      auto-save-interval 200
      backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      default-directory "~/"
      delete-by-moving-to-trash t
      delete-old-versions t
      indent-tabs-mode nil
      inhibit-startup-screen t
      kept-new-versions 6
      load-prefer-newer t
      message-log-max 1000
      ns-pop-up-frames nil
      ns-use-srgb-colorspace t
      require-final-newline t
      ring-bell-function 'ignore
      save-interprogram-paste-before-kill t
      show-paren-style 'parenthesis
      tab-width 4
      version-control t)

(unless (eq 'darwin system-type)
  (menu-bar-mode -1))

(blink-cursor-mode -1)
(column-number-mode +1)
(scroll-bar-mode -1)
(show-paren-mode +1)
(tool-bar-mode -1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defalias 'yes-or-no-p 'y-or-n-p)

(when (and (eq 'darwin system-type)
           (file-exists-p "/usr/local/bin/gls"))
  (setq insert-directory-program "/usr/local/bin/gls"))


(use-package apropos
  :defer t
  :config
  (setq apropos-do-all t))

(use-package auth-source
  :defer t
  :config
  (add-to-list 'auth-sources 'macos-keychain-internet))

(use-package company
  :ensure t
  :init (global-company-mode))

(use-package dired
  :defer t
  :config
  (setq dired-listing-switches "-alh"))


(use-package exec-path-from-shell
  :if (eq 'darwin system-type)
  :defer 1
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "DOKTORGRAD"))

(use-package helm
  :ensure t
  :defer 0.1
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files))
  :config
  (use-package helm-projectile
    :ensure t)
  (helm-mode +1)
  (helm-projectile-on))

(use-package highlight-symbol
  :ensure t
  :commands highlight-symbol-mode
  :config
  (setq highlight-symbol-idle-delay 0.2)
  (add-hook 'highlight-symbol-mode-hook
            (function
             (lambda () (highlight-symbol-nav-mode +1)))))

(use-package ispell
  :defer t
  :config
  (setq ispell-dictionary "english"
        ispell-highlight-face 'flyspell-incorrect
        ispell-program-name "/usr/local/bin/aspell"))

(use-package noflet
  :ensure t
  :commands noflet)

(use-package perspective
  :ensure t
  :commands persp-mode
  :config
  (set-face-attribute 'persp-selected-face nil :foreground "#81a2be"))

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :bind ("s-p" . projectile-command-map)
  :config
  (persp-mode)
  (use-package persp-projectile
    :ensure t
    :commands persp-projectile
    :config
    (add-hook 'persp-activated-hook
	      #'(lambda ()
		  (persp-add-buffer
		   (get-buffer-create "*Messages*")))))
  (require 'persp-projectile))

(use-package saveplace
  :preface (setq-default save-place t)
  :config
  (setq save-place-file "~/.emacs.d/.places"))

(use-package smartparens
  :ensure t
  :init
  :commands (smartparens-mode
	     smartparens-strict-mode)
  :config
  (require 'smartparens-config)

  (defun ljos/sp-wrap-with-parens ())
  (bind-keys
   :map smartparens-strict-mode-map
   ("C-}" . sp-forward-slurp-sexp)
   ("M-s" . sp-backward-unwrap-sexp)
   ("C-c [" . sp-select-next-thing)
   ("C-c ]" . sp-select-next-thing-exchange)))

(use-package smart-mode-line
  :ensure t
  :init
  (setq-default sml/vc-mode-show-backend t
		sml/theme 'respectful)
  (sml/setup))

(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh"))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-min-dir-content 2))

(use-package visual-regexp
  :ensure t
  :bind (("C-c q" . vr/query-replace)
         ("C-c r" . vr/replace)))

(use-package window-number
  :commands window-number-mode
  :load-path "site-lisp/"
  :init (window-number-mode))

(use-package clojure-mode
  :ensure t
  :init
  (use-package cider
    :ensure t
    :commands cider-minor-mode
    :init
    (add-hook 'cider-mode-hook #'eldoc-mode)
    (add-hook 'cider-mode-hook #'smartparens-strict-mode))
  (add-hook 'clojure-mode-hook #'cider-minor-mode)
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode))

(use-package ess-site
  :ensure ess
  :mode ("\\.R\\'" . R-mode)
  :commands R
  :config
  (add-hook 'R-mode-hook #'subword-mode)
  (add-hook 'R-mode-hook #'smartparens-strict-mode))

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode))


(use-package lisp-mode
  :bind (([C-s-268632091] . backward-sexp)
         ([C-s-268632093] . forward-sexp))
  :config
  (use-package elisp-slime-nav
    :ensure t
    :commands elisp-slime-nav-mode)
  (use-package macrostep
    :ensure t
    :bind ("C-c e" . macrostep-expand))

  (use-package slime
    :ensure t
    :commands (slime slime-lisp-mode-hook)
    :config
    (add-to-list 'slime-contribs 'slime-fancy)
    (slime-setup)
    (add-hook 'slime-repl-mode-hook #'smartparens-strict-mode))

  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  (add-hook 'ielm-mode-hook #'elisp-slime-nav-mode)
  (add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook
            #'(lambda ()
		(sp-local-pair 'minibuffer-mode "'" nil :actions nil)
		(smartparens-strict-mode +1)))

  (add-hook 'lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'lisp-mode-hook #'slime-lisp-mode-hook)

  (setq inferior-lisp-program "sbcl"))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-push-always-verify nil))

(use-package org
  :load-path "site-lisp/org-mode/lisp"
  :mode ("\\.org'" . org-mode)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (awk . t)
     (sed . t)
     (sh . t)))
  (bind-key (kbd "C-c a") 'org-archive-to-archive-sibling org-mode-map)
  (setq org-export-with-section-numbers nil
        org-export-with-toc nil
        org-src-fontify-natively t
        org-src-window-setup 'current-window
        org-startup-folded 'showall
        org-use-speed-commands t)
  (add-hook 'org-mode-hook #'(lambda () (auto-fill-mode +1)))
  (use-package ob-shell
    :commands org-babel-execute:bash
    :config
    (add-to-list 'org-babel-default-header-args:sh
		 ,(:prologue . `(concat ". " (expand-file-name "~/.bashrc")))))

  (use-package ox-latex
    :config
    (setq org-latex-pdf-process '("latexmk -gg -pdf -bibtex %f"))

    (unless (boundp 'org-latex-packages-alist)
      (setq org-latex-packages-alist nil))

    (add-to-list 'org-latex-packages-alist '("" "microtype"))
    (add-to-list 'org-latex-packages-alist '("l2tabu" "nag"))
    (add-to-list 'org-latex-packages-alist '("" "lmodern") 't)))


(use-package prolog
  :ensure t
  :mode ("\\.pl" . prolog-mode))

(use-package python
  :ensure t
  :mode ("\\.py" . python-mode)
  :config
  (use-package elpy
    :ensure t
    :commands elpy-enable
    :config
    (setq elpy-rpc-python-command "python3"
	  elpy-modules (dolist (elem '(elpy-module-highlight-indentation
				       elpy-module-yasnippet))
			 (remove elem elpy-modules)))
    (elpy-use-ipython))
  (elpy-enable))

(use-package sed-mode
  :load-path "site-lisp/sed-mode"
  :mode "\\.sed\\'")

(use-package simple
  :config
  (add-hook 'prog-mode-hook
            #'(lambda () (highlight-symbol-mode +1))))

(use-package sparql-mode
  :load-path "site-lisp/sparql-mode"
  :mode "\\.sparql$"
  :defines sparql-default-base-url
  :config
  (setq sparql-default-base-url "http://live.dbpedia.org/sparql"))


;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
