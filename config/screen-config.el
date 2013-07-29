;;; Found at http://stackoverflow.com/a/94277
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    (add-to-list 'default-frame-alist
                 (cons 'width
                       (if (> (x-display-pixel-width) 1280)
                           100 80)))
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 50)
                          (frame-char-height)))))))

(defun x-maximize-frame ()
    (interactive)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(if (eq 'x window-system)
    (x-maximize-frame)
  (set-frame-size-according-to-resolution))

(set-frame-position (next-frame) 0 0)

(provide 'screen-config)
