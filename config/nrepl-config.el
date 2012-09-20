(require 'nrepl)
(add-hook 'nrepl-mode-hook 'enable-paredit-mode)

(setq nrepl-popup-stacktraces nil)
(setq nrepl-history-file "~/.emacs.d/history/nrepl")

(provide 'nrepl-config)
