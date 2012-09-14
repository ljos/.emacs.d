(require 'nrepl)
(add-hook 'nrepl-mode-hook 'enable-paredit-mode)

(provide 'nrepl-config)
