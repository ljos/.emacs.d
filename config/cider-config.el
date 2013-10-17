(require 'cider-autoloads)

(autoload 'nrepl-connect "nrepl"
  "nrepl mode" t nil)

;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode))

(eval-after-load "nrepl"
  '(progn
     (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
     (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
     (add-hook 'cider-repl-mode-hook 'subword-mode)
     (setq nrepl-hide-special-buffers t)
     (setq nrepl-popup-stacktraces-in-repl t)
     (setq nrepl-history-file "~/.emacs.d/history/nrepl")
     (define-key nrepl-interaction-mode-map (kbd "C-c C-n") 'nrepl-set-ns)))

;; (require 'nrepl-ritz-autoloads)

;; (eval-after-load "nrepl-ritz"
;;   '(progn
;;      (define-key cider-interaction-mode-map (kbd "C-c C-j" 'nrepl-javadoc))
;;      (define-key cider-repl-mode-map (kbd "C-c C-j" 'nrepl-javadoc))
;;      (define-key cider-interaction-mode-map (kbd "C-c C-a" 'nrepl-apropos))
;;      (define-key cider-repl-mode-map (kbd "C-c C-a" 'nrepl-apropos))))

(provide 'cider-config)
