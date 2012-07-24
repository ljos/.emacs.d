(set-default-font "Monaco-14")

(set-fringe-mode '(4 . 4))
(blink-cursor-mode nil)
(global-linum-mode t)

(let ((themes-folder
       (expand-file-name "themes"
                         (file-name-directory
                          (locate-library "color-theme")))))
  (if (not (file-exists-p themes-folder))
      (make-directory themes-folder)))


(when (window-system)
  (require 'color-theme)
  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-color-theme-solarized/")
  (load "color-theme-solarized.el")
  (load-theme 'solarized-light t)
  (setq color-theme-is-global t))

(provide 'face-config)
