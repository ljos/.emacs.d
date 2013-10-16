(require 'js2-mode-autoloads)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(eval-after-load "js2-mode"
  '(progn
     (setq-default js2-global-externs '("_"))
     (add-hook 'js2-mode-hook
               '(lambda () (setq ac-sources
                                 (cons ac-source-yasnippet
                                       ac-sources))))
     (add-hook 'js2-mode-hook 'turn-on-smartparens-mode)))



(provide 'js2mode-config)
