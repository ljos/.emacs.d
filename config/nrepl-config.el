(require 'nrepl)
(add-hook 'nrepl-mode-hook 'enable-paredit-mode)

(setq nrepl-popup-stacktraces nil)
(setq nrepl-history-file "~/.emacs.d/history/nrepl")

(defun nrepl-local ()
  (interactive)
  (nrepl-connect "localhost" "50001"))

(define-key nrepl-interaction-mode-map (kbd "C-c C-n") 'nrepl-set-ns)

(provide 'nrepl-config)
