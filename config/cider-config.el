(require 'cider-autoloads)


(eval-after-load "cider"
  '(progn
     (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
     (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
     (add-hook 'cider-repl-mode-hook 'subword-mode)
     (setq cider-history-file "~/.emacs.d/history/nrepl")))

(provide 'cider-config)
