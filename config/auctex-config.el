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

(defadvice TeX-next-error (before set-tex-help-buffer-w)
  (let ((tex-help (get-buffer "*TeX Help*")))
    (when tex-help
      (read-only-mode nil))))

(ad-activate 'TeX-next-error)

(defadvice TeX-error (after set-tex-help-buffer-ro)
  (let ((tex-help (get-buffer "*TeX Help*")))
    (when tex-help
      (with-current-buffer tex-help
        (read-only-mode t)
        (local-set-key (kbd "q") 'delete-window)))))

(ad-activate 'TeX-error)

(defadvice TeX-parse-reset (after make-master-file-default () activate)
  (push (concat (substring (buffer-name) 1 (- (length (buffer-name)) 8))
                "."
                TeX-default-extension)
        TeX-error-file)
  (push nil TeX-error-offset))

(ad-activate 'TeX-parse-reset)

(provide 'auctex-config)
