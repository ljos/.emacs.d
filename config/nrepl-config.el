(require 'nrepl-autoloads)
(require 'ac-nrepl-autoloads)
(autoload 'nrepl-connect "nrepl"
  "nrepl mode" t nil)

(add-hook 'nrepl-mode 'ac-nrepl-setup)
(add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(add-hook 'nrepl-mode-hook 'enable-paredit-mode)

(eval-after-load 'nrepl
  '(progn
     (define-key nrepl-interaction-mode-map (kbd "C-c C-n") 'nrepl-set-ns)
     (setq nrepl-history-file "~/.emacs.d/history/nrepl")))

(defun nrepl-local ()
  (interactive)
    (nrepl-connect "localhost" "50001"))

(provide 'nrepl-config)
