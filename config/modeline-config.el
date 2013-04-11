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

(make-face 'mode-line-minor-mode-face)

(set-face-attribute 'mode-line nil
  :box '(:line-width 1
         :color "gray25"))

(set-face-attribute 'mode-line-minor-mode-face nil
  :inherit 'mode-line-face
  :height 110)

(defun center-string-in-char (str len char)
  (store-substring (make-string len char)
                   (/ (- len (length str)) 2) str))

(setq-default mode-line-position
              '(" %03l:%2c"))

(setq-default mode-line-format
  '("%e "

    "%*"                                ; file status

    (:eval
     (propertize
      (if (buffer-narrowed-p)
          " 狭")))

    mode-line-position
    "  "
    (:eval
     (propertize                        ; file/buffer name
      (center-string-in-char
       (truncate-string-to-length
        (or buffer-file-truename
            (buffer-name))
        20 nil nil "..")
       20
       ?\s)
      'help-echo (buffer-file-name)     ; echo full name
      'local-map
      (let ((map (make-sparse-keymap)))
        (define-key map [mode-line mouse-3]
          'mode-line-next-buffer)
        (define-key map [mode-line mouse-1]
          'mode-line-previous-buffer)
        map)))

    "  "

    (:eval
     (propertize mode-name
                 'help-echo (format-mode-line minor-mode-alist)))
    " "
    vc-mode
    "  "

    pomodoro-mode-line-string

    (:eval
     (concat
      (propertize " " 'display
                 `((space :align-to
                           (- right ,(if (string= "" pomodoro-mode-line-string) 20 8)))))
      (propertize (if (string= "" pomodoro-mode-line-string)
                      (format-time-string " %a %b %d, %H:%M")
                    (format-time-string " %H:%M"))                 ; time
                  'help-echo
                  (format-time-string "%A, %B %d, %Y, %H:%M"))))))

(provide 'modeline-config)
