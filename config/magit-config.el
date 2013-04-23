(require 'magit-autoloads)
(require 'magithub-autoloads)

(eval-after-load 'magit-log-edit-mode
  '(define-key magit-log-edit-mode-map (kbd "C-x C-s") 'magit-log-edit-commit))

(provide 'magit-config)
