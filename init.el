(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; When opening a new file from OS do not create a new frame.
(setq ns-pop-up-frames 'nil)

(setq message-log-max t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(fset 'yes-or-no-p 'y-or-n-p)
(winner-mode t)

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
(require 'evil-config)
(require 'expandregion-config)
(require 'face-config)
(require 'ffip-config)
(require 'flyspell-config)
(require 'gist-config)
(require 'go-config)
(require 'ielm-config)
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
(require 'slime-config)
(require 'sws-config)
(require 'tramp-config)
(require 'undotree-config)
(require 'yasnippet-config)

(setq visible-bell nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(show-paren-mode t)
 '(menu-bar-mode t)
 '(tool-bar-mode nil))
