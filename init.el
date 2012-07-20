(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

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
(load "paredit-config.el") ;needs to be before elpa as we need to fix a few things
(load "elpa-config.el")
(load "face-config.el")
(load "autocomplete-config.el")
(load "browsekillring-config.el")
(load "undotree-config.el")
(load "modeline-config.el")
(load "writegood-config.el")
(load "acejump-config.el")
(load "org-config.el")
(load "slimerepl-config.el")
(load "clojure-config.el")
(load "auctex-config.el")
(load "tramp-config.el")
(load "prolog-config.el")
(load "gist-config.el")
(load "langtool-config.el")
(load "minibuffer-config.el")
(load "screen-config.el") ;This should be the last to happen


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
