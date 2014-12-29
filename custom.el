(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apropos-do-all t)
 '(auth-sources
   (quote
    ("~/.authinfo" "~/.authinfo.gpg" "~/.netrc" macos-keychain-internet)))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(before-save-hook (quote (delete-trailing-whitespace)))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message "bjarte")
 '(inhibit-startup-screen t)
 '(ispell-dictionary "english")
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-program-name "/usr/local/bin/aspell")
 '(load-prefer-newer t)
 '(message-log-max 1000)
 '(ns-pop-up-frames nil)
 '(ns-use-srgb-colorspace t)
 '(org-structure-template-alist
   (quote
    (("s" "#+BEGIN_SRC ?

#+END_SRC" "<src lang=\"?\">

</src>")
     ("e" "#+BEGIN_EXAMPLE
?
#+END_EXAMPLE" "<example>
?
</example>")
     ("q" "#+BEGIN_QUOTE
?
#+END_QUOTE" "<quote>
?
</quote>")
     ("v" "#+BEGIN_VERSE
?
#+END_VERSE" "<verse>
?
</verse>")
     ("V" "#+BEGIN_VERBATIM
?
#+END_VERBATIM" "<verbatim>
?
</verbatim>")
     ("c" "#+BEGIN_CENTER
?
#+END_CENTER" "<center>
?
</center>")
     ("l" "#+BEGIN_LaTeX
?
#+END_LaTeX" "<literal style=\"latex\">
?
</literal>")
     ("L" "#+LaTeX: " "<literal style=\"latex\">?</literal>")
     ("h" "#+BEGIN_HTML
?
#+END_HTML" "<literal style=\"html\">
?
</literal>")
     ("H" "#+HTML: " "<literal style=\"html\">?</literal>")
     ("a" "#+BEGIN_ABSTRACT
?
#+END_ABSTRACT" "")
     ("A" "#+ASCII: " "")
     ("i" "#+INDEX: ?" "#+INDEX: ?")
     ("I" "#+INCLUDE: %file ?" "<include file=%file markup=\"?\">"))))
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/.recentf")
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((eval load-file
           (concat
            (projectile-project-root)
            ".custom.el"))
     (eval load-file ".custom.el")
     (eval progn
           (setq ess-directory
                 (projectile-project-root)
                 default-directory
                 (projectile-project-root))
           (advice-add
            (quote org-babel-tangle)
            :around
            (function
             (lambda
               (f &rest args)
               (let
                   ((default-directory
                      (concat
                       (projectile-project-root)
                       (file-name-as-directory "src"))))
                 (apply f args))))))
     (eval setq ess-directory
           (projectile-project-root)
           default-directory
           (projectile-project-root))
     (org-confirm-babel-evaluate)
     (ess-ask-for-ess-directory)
     (eval setq ess-directory
           (projectile-project-root))
     (eval setq ljos/project-root
           (projectile-project-root))
     (ljos/project-directory eval projectile-project-root))))
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/.places")
 '(savehist-additional-variables
   (quote
    (quote
     (search-ring regexp-search-ring global-mark-ring mark-ring))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-min-dir-content 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(persp-selected-face ((t (:foreground "#81a2be")))))
