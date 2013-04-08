(autoload 'dired-single-buffer "dired-single" "" t)
(autoload 'dired-single-buffer-mouse "dired-single" "" t)
(autoload 'dired-single-magic-buffer "dired-single" "" t)
(autoload 'dired-single-toggle-buffer-name "dired-single" "" t)


(if  (not (boundp 'dired-mode-map))
  (add-hook 'dired-load-hook
            (lambda ()
              (define-key dired-mode-map [return]
                'dired-single-buffer)
              (define-key dired-mode-map [mouse-1]
                'dired-single-buffer-mouse)
              (define-key dired-mode-map "^"
                (function
                 (lambda ()
                   (interactive)
                   (dired-single-buffer "..")))))))

(provide 'dired-config)
