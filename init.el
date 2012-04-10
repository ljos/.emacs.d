(set-default-font "Monaco-14")
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 100))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 50)
                          (frame-char-height)))))))

(set-frame-size-according-to-resolution)

;;; Some path stuff
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
  (dolist (dir exec-paths) (add-to-path dir)))

(initialize-exec-path)

;;; ELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
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

;;;;Auto-complete
(require 'auto-complete-config)
(ac-config-default)

;;; Auctex-mode
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;;;color-theme
; need to get our own as the one in the repo doesn't work properly
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized/")
(load "color-theme-solarized.el")
(load-theme 'solarized-light t)
(setq color-theme-is-global t)

;;; Org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))  
(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))
(add-to-list 'org-export-latex-classes
             `("thesis"
               "\\documentclass{report}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(require 'org-special-blocks)

;;;;; modeline edits
(setq mode-line-in-non-selected-windows nil)
(setq default-mode-line-format
      '(" "
        mode-line-mule-info
        mode-line-modified
        "   "
        (:propertize "%b " 'face 'font-lock-keyword-face
                     'help-echo (buffer-file-name))
        "   " 
        (:propertize "%l" 'face 'font-lock-type-face) ","
        (:propertize "%c" 'face 'font-lock-type-face) 
        "    ["
        (:propertize mode-name
                     help-echo (format-mode-line minor-mode-alist))
        (:propertize mode-line-process)
        "]    "
        (:propertize global-mode-string)
        "   "
        "-%-"))

;;; DIV configs
(global-linum-mode 1)
(setq visible-bell nil)
(menu-bar-mode)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq display-time-24hr-format 't)
(setq display-time-day-and-date 't)
(setq display-time-default-load-average nil)

(display-time-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(fringe-mode (quote (4 . 4)) nil (fringe))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
