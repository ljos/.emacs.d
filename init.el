(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(add-hook 'after-init-hook '(lambda () (progn (setq visible-bell nil)
                                         (menu-bar-mode t))))

(add-to-list 'load-path "~/.emacs.d/config/")
(load "elpa-config.el")
(load "face-config.el")
(load "modeline-config.el")
(load "path-config.el")
(load "writegood-config.el")
(load "org-config.el")
(load "clojure-config.el")
(load "paredit-config.el")
(load "auctex-config.el")
(load "screen-config.el")

