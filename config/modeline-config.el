(setq display-time-24hr-format 't)
(setq display-time-day-and-date 't)
(setq display-time-default-load-average nil)
(display-time-mode 't)

(setq mode-line-in-non-selected-windows nil)

(defun truncate-string-to-length (str end-column
                                      &optional start-column padding ellipsis)
  "The same as truncate-string-to-width,
except it truncates from the start of the list"
  (concat
   (reverse
    (append
     (truncate-string-to-width
      (concat (reverse (append (format  str) nil)))
      end-column start-column padding ellipsis) nil))))

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

(setq default-mode-line-format
      '(" "
        mode-line-mule-info
        mode-line-modified
        " "
        (:propertize evil-mode-line-tag)
        " "  
        (:eval (propertize 
                (format "%-20s"
                        (truncate-string-to-length (or buffer-file-truename
                                                       (buffer-name))
                                                   20 nil nil ".."))
                           'help-echo (buffer-file-name)
                           'local-map
                           (let ((map (make-sparse-keymap)))
                             (define-key map [mode-line mouse-3]
                               'mode-line-next-buffer)
                             (define-key map [mode-line mouse-1]
                               'mode-line-previous-buffer)
                             map)))
        "  " 
        (:propertize "%02l")
        ","
        (:propertize "%02c" )
        "  "
        (:eval (propertize
                (format "%-15s"
                        (concat "["
                                (short-major-mode-name mode-name)
                                "]"))
                'help-echo
                '(concat
                  mode-name ": "
                  (format-mode-line minor-mode-alist))))
        ;; (:propertize mode-line-process)
        (:eval (propertize (format-time-string "%a %b %d, %H:%M")
                           'help-echo (format-time-string
                                       "%A, %B %d, %Y, %H:%M")))
        "  "
        (:propertize pomodoro-mode-line-string)
        "-%-"))

(provide 'modeline-config)
