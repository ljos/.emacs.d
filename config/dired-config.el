(autoload 'dired-single-buffer "dired-single" "" t)
(autoload 'dired-single-buffer-mouse "dired-single" "" t)
(autoload 'dired-single-magic-buffer "dired-single" "" t)
(autoload 'dired-single-toggle-buffer-name "dired-single" "" t)

(if (boundp 'dired-mode-map)
    (my-dired-init)
  (add-hook 'dired-load-hook
            ( lambda ()
              (define-key dired-mode-map [return] 'dired-single-buffer)
              (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
              (define-key dired-mode-map "^"
                (function
                 (lambda nil (interactive) (dired-single-buffer "..")))))))

(provide 'dired-config)
