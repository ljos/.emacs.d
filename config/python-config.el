(autoload 'python-mode "python-mode" "Python mode" t nil)
(autoload 'ipython "ipython" "Interactive python" t nil)
(require 'python-pep8-autoloads)
(require 'python-pylint-autoloads)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(provide 'python-config)
