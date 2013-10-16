(require 'smartparens-autoloads)

(eval-after-load "smartparens"
  '(progn
     (define-key sp-keymap (kbd "C-k")
       '(lambda (arg)
          (interactive "P")
          (let ((p (point)))
            (sp-end-of-sexp)
            (kill-region p (point)))))))



(provide 'smartparens-conf)
