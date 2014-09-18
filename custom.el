(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(before-save-hook (quote (delete-trailing-whitespace)))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message "bjarte")
 '(inhibit-startup-screen t)
 '(ispell-dictionary "english")
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-program-name "/usr/local/bin/aspell")
 '(message-log-max 1000)
 '(ns-pop-up-frames nil)
 '(ns-use-srgb-colorspace t)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/.recentf")
 '(safe-local-variable-values
   (quote
    ((org-confirm-babel-evaluate)
     (ess-ask-for-ess-directory)
     (eval setq ess-directory
           (projectile-project-root))
     (eval setq ljos/project-root
           (projectile-project-root))
     (ljos/project-directory eval projectile-project-root))))
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/.places")
 '(savehist-additional-variables (quote (quote (search-ring regexp-search-ring global-mark-ring mark-ring))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-min-dir-content 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
