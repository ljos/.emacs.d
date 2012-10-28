(autoload 'python-mode "python-mode" "Python mode" t nil)
(autoload 'ipython "ipython" "Interactive python" t nil)
(require 'python-pep8-autoloads)
(require 'python-pylint-autoloads)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(add-hook 'python-mode-hook 'esk-paredit-nonlisp)
(add-hook 'python-mode-hook 'enable-paredit-mode)

(eval-after-load 'python-mode
  '(progn
     (define-key python-mode-map "{" 'paredit-open-curly)
     (define-key python-mode-map "}" 'paredit-close-curly)
     (define-key python-mode-map "'" 'paredit-singlequote)))

(provide 'python-config)

