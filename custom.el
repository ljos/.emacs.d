(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(before-save-hook (quote (delete-trailing-whitespace)))
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(focus-follows-mouse nil)
 '(fringe-mode (quote (0)) nil (fringe))
 '(global-hl-line-mode t)
 '(indent-tabs-mode t)
 '(global-linum-mode nil)
 '(inhibit-startup-echo-area-message "bjarte")
 '(inhibit-startup-screen t)
 '(keyboard-coding-system (quote utf-8-unix))
 '(linum-format (quote ljos/linum-format))
 '(message-log-max t)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
 '(ns-pop-up-frames nil)
 '(projectile-enable-caching t)
 '(projectile-global-mode t)
 '(projectile-tags-command "/usr/local/bin/ctags -Re %s")
 '(require-final-newline t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(use-dialog-box nil)
 '(visible-bell nil)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :background "#282a2e" :foreground "#969896")))))
