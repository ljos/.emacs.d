(if (and (window-system)
         (string= "darwin"
                  system-type))
    (set-face-attribute 'default nil :font "menelo-14")
  (set-face-attribute 'default nil :font (font-get-system-font)))

(set-fringe-mode '(0 . 6))
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
  (require 'color-theme-sanityinc-tomorrow))

(provide 'face-config)
