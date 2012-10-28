(eval-after-load "ielm"
  '(progn
     (add-hook 'ielm-mode-hook (lambda () (paredit-mode +1)))))

(provide 'ielm-config)
