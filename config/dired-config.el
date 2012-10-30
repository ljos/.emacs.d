(autoload 'joc-dired-single-buffer "dired-single" "" t)
(autoload 'joc-dired-single-buffer-mouse "dired-single" "" t)
(autoload 'joc-dired-single-magic-buffer "dired-single" "" t)
(autoload 'joc-dired-single-toggle-buffer-name "dired-single" "" t)

(if (boundp 'dired-mode-map)
    (my-dired-init)
  (add-hook 'dired-load-hook
            ( lambda ()
              (define-key dired-mode-map [return]
                'joc-dired-single-buffer)
              (define-key dired-mode-map [mouse-1]
                'joc-dired-single-buffer-mouse)
              (define-key dired-mode-map "^"
                (function
                 (lambda ()
                   (interactive)
                   (joc-dired-single-buffer "..")))))))

(provide 'dired-config)
