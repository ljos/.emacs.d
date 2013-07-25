(require 'auctex-autoloads)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (setq TeX-view-program-list '(("Shell Default" "open %o")))
             (setq TeX-view-program-selection '((output-pdf "Shell Default")))))
(provide 'auctex-config)
