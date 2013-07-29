(require 'hy-mode)

(eval-after-load "hy-mode"
  '(progn
     (add-hook 'hy-mode-hook 'paredit-mode)))

(provide 'hy-config)
