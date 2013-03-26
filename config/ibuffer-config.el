(require 'ibuffer)
(setq ibuffer-saved-filter-groups
  (quote (("default"
            ("Org" ;; all org-related buffers
              (mode . org-mode))
            ("Programming"
              (or
                (mode . c-mode)
                (mode . clojure-mode)
                (mode . emacs-lisp-mode)
                (mode . go-mode)
                (mode . perl-mode)
                (mode . python-mode)
                ))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-expert t)

(provide 'ibuffer-config)
