(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq message-log-max t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

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

;;;The config files.
(add-to-list 'load-path "~/.emacs.d/config/")
(require 'paredit-config) ;needs to be before elpa as we need to fix a few things
(require 'elpa-config)
(require 'face-config)
(require 'browsekillring-config)
(require 'undotree-config)
(require 'modeline-config)
(require 'acejump-config)
(require 'org-config)
(require 'slimerepl-config)
(require 'clojure-config)
(require 'nrepl-config)
(require 'acnrepl-config)
(require 'auctex-config)
(require 'tramp-config)
(require 'prolog-config)
(require 'minibuffer-config)
(autoload 'langtool-check-buffer "langtool-config.el" "Langtool config" t)
(require 'linum-config)
(require 'gist-config)
(require 'applescript-config)
(require 'slime-config)
(require 'autocomplete-config) ;; needs to after a couple of things.
(require 'screen-config) ;This should be the last to happen


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
