(require 'nrepl-autoloads)
(require 'ac-nrepl-autoloads)

(autoload 'nrepl-connect "nrepl"
  "nrepl mode" t nil)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(eval-after-load "nrepl"
  '(progn
     (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
     (add-hook 'nrepl-mode-hook 'enable-paredit-mode)
     (add-hook 'nrepl-mode-hook 'subword-mode)
     (setq nrepl-hide-special-buffers t)
     (setq nrepl-popup-stacktraces-in-repl t)
     (setq nrepl-history-file "~/.emacs.d/history/nrepl")
     (define-key nrepl-interaction-mode-map (kbd "C-c C-n") 'nrepl-set-ns)
     (load-file "~/.emacs.d/site-lisp/javert/nrepl-inspect.el")
     (define-key nrepl-mode-map (kbd "C-c i") 'nrepl-inspect)
     ))

(defun nrepl-local ()
  (interactive)
  (nrepl-connect "localhost" "50001"))

(require 'nrepl-ritz-autoloads)

(eval-after-load "nrepl-ritz"
  '(progn
     (define-key nrepl-interaction-mode-map (kbd "C-c C-j" 'nrepl-javadoc))
     (define-key nrepl-mode-map (kbd "C-c C-j" 'nrepl-javadoc))
     (define-key nrepl-interaction-mode-map (kbd "C-c C-a" 'nrepl-apropos))
     (define-key nrepl-mode-map (kbd "C-c C-a" 'nrepl-apropos))))

(provide 'nrepl-config)
