(set-face-attribute 'default nil :font "monaco-14")

(set-fringe-mode '(0 . 6))
(blink-cursor-mode nil)
(global-linum-mode t)

(let ((themes-folder
       (expand-file-name "themes"
                         (file-name-directory
                          (locate-library "color-theme")))))
  (if (not (file-exists-p themes-folder))
      (make-directory themes-folder)))

(add-to-list 'load-path "~/.emacs.d/site-lisp/tomorrow-theme/")

(when (window-system)
  (require 'color-theme)
  ;; (require 'color-theme-solarized)
  ;; (load-theme 'solarized-light t)
  (require 'color-theme-tomorrow)
  (color-theme-tomorrow-night)
  (setq color-theme-is-global t))

(provide 'face-config)
