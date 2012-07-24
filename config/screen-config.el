;;; Found at http://stackoverflow.com/a/94277
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 100))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 50)
                          (frame-char-height)))))))

(set-frame-size-according-to-resolution)

(provide 'screen-config)
