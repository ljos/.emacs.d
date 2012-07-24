(setq display-time-24hr-format 't)
(setq display-time-day-and-date 't)
(setq display-time-default-load-average nil)
(display-time-mode 't)

(setq mode-line-in-non-selected-windows nil)

;;Heavily edited from http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
(setq default-mode-line-format
      '(" "
        mode-line-mule-info
        mode-line-modified
        "   "
        (:propertize "%b " 'face 'font-lock-keyword-face
                     'help-echo (buffer-file-name))
        "   " 
        (:propertize "%l" 'face 'font-lock-type-face) ","
        (:propertize "%c" 'face 'font-lock-type-face) 
        "    ["
        (:propertize mode-name
                     help-echo (format-mode-line minor-mode-alist))
        (:propertize mode-line-process)
        "]    "
        (:propertize global-mode-string)
        "   "
        "-%-"))

(provide 'modeline-config)
