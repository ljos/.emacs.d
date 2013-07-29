(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

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

(setq-default color-theme-is-global t
              backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
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

(global-set-key (kbd "C-c f") 'find-file-in-project)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(define-key 'help-command "a" 'apropos)

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
(require 'elisp-config)
(require 'elispslimenav-config)
;(require 'evil-config)
;(require 'emacseclim-config)
(require 'expandregion-config)
(require 'face-config)
(require 'ffap-config)
(require 'ffip-config)
(require 'gist-config)
(require 'go-config)
(require 'hy-config)
(require 'ibuffer-config)
(require 'ido-config)
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
(require 'prog-config)
(require 'python-config)
(require 'saveplace-config)
(require 'screenplay-config)
(require 'screen-config)
(require 'simplehttpd-config)
(require 'skewer-config)
(require 'slime-config)
(require 'smex-config)
(require 'sparql-config)
(require 'sws-config)
(require 'tramp-config)
(require 'undotree-config)
(require 'yasnippet-config)

;; Convenience function to get all keys in a hash table.
(defun keys (hashtable)
  "Return all keys in hashtable."
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys))) hashtable)
    allkeys))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
