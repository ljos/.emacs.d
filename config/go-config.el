(require 'go-mode-autoloads)


(eval-after-load "go-mode"
  '(progn
     (define-key go-mode-map "{" 'paredit-open-curly)
     (define-key go-mode-map "}" 'paredit-close-curly-and-newline)
     (add-hook 'go-mode-hook 'esk-paredit-nonlisp)))

(provide 'go-config)
