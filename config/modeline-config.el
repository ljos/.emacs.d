(require 'powerline)

(setq mode-line-in-non-selected-windows nil)

(defun truncate-string-to-length (str end-column &optional start-column padding ellipsis)
  "The same as truncate-string-to-width,
except it truncates from the start of the list"
  (concat
   (reverse
    (append (truncate-string-to-width
             (concat (reverse (append (format  str) nil)))
             end-column start-column padding ellipsis)
            nil))))

(defun short-major-mode-name (str)
  (if (stringp str)
      (cond ((string= str "Lisp Interaction")
             "elI")
            ((string= str "JavaScript-IDE")
             "JS-IDE")
            ((string= str "ClojureScript")
             "CljS")
            (t str))
    str))

(setq-default mode-line-format
  '("%e"
    (:eval
     (let* ((active (eq (frame-selected-window) (selected-window)))
            (face1 (if active 'powerline-active1 'powerline-inactive1))
            (face2 (if active 'powerline-active2 'powerline-inactive2))
            (lhs (list
                  (powerline-raw "%*" nil 'l)
                                        ;                  (propertize evil-mode-line-tag)
                  " "
                  (propertize
                   (concat (format "%-20s"
                                   (truncate-string-to-length (or buffer-file-truename
                                                                  (buffer-name))
                                                              20 nil nil "..")))
                   'help-echo (buffer-file-name)
                   'local-map
                   (let ((map (make-sparse-keymap)))
                     (define-key map [mode-line mouse-3]
                       'mode-line-next-buffer)
                     (define-key map [mode-line mouse-1]
                       'mode-line-previous-buffer)
                     map))
                  (powerline-raw " ")
                  (powerline-arrow-right nil face1)
                  (powerline-raw " %03l:%2c" face1 'r)
                  (powerline-arrow-right face1 face2)
                  (powerline-major-mode face2 'l)
                  (powerline-raw mode-line-process face2 'l)
                  (powerline-narrow face1 'l)
                  (powerline-arrow-right face2 face1)
                  (powerline-vc face1)))
            (rhs (list
                  (powerline-raw global-mode-string face1 'r)
                  (powerline-raw pomodoro-mode-line-string face1)
                  (powerline-arrow-right face1 nil)
                  (propertize (format-time-string " %a %b %d, %H:%M")
                              'help-echo (format-time-string
                                          "%A, %B %d, %Y, %H:%M"))
                  (powerline-raw "  "))))
       (concat
        (powerline-render lhs)
        (powerline-fill face1 (+ 2 (powerline-width rhs)))
        (powerline-render rhs))))))

(provide 'modeline-config)
