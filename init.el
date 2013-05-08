(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; When opening a new file from OS do not create a new frame.
(setq ns-pop-up-frames 'nil)

(setq message-log-max t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(winner-mode t)

(setq require-final-newline t)
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

;;;The config files.
(add-to-list 'load-path "~/.emacs.d/config/")
;; elpa needs to be first if some packages are missing.
(require 'elpa-config)
(require 'acejump-config)
(require 'auctex-config)
(require 'autocomplete-config)
(require 'browsekillring-config)
(require 'clojure-config)
(require 'clojurescript-config)
(require 'cmdfrequency-config)
(require 'coffee-config)
(require 'dired-config)
(require 'elispslimenav-config)
;(require 'evil-config)
;(require 'emacseclim-config)
(require 'expandregion-config)
(require 'face-config)
(require 'ffip-config)
(require 'gist-config)
(require 'go-config)
(require 'ibuffer-config)
(require 'ielm-config)
(require 'ispell-config)
(require 'jade-config)
(require 'js2mode-config)
(require 'langtool-config)
(require 'linum-config)
(require 'magit-config)
(require 'minibuffer-config)
(require 'modeline-config)
(require 'move-config)
(require 'multiplecursor-config)
(require 'multiweb-config)
(require 'nrepl-config)
(require 'org-config)
(require 'paredit-config)
(require 'php+-config)
(require 'pomodoro-config)
(require 'popwin-config)
(require 'prolog-config)
(require 'python-config)
(require 'screen-config)
(require 'screenplay-config)
(require 'slime-config)
(require 'sparql-config)
(require 'sws-config)
(require 'tramp-config)
(require 'undotree-config)
;(require 'workgroups-config)
(require 'yasnippet-config)

;; Redefining this so we have the font-lock on wordbounderies instead
;; of as it is in starter-kit.
(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)\\b"
          1 font-lock-warning-face t))))

(setq visible-bell nil)
(setq ring-bell-function #'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

(fringe-mode 0)
;(set-display-table-slot standard-display-table 'wrap ?↩)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(menu-bar-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
