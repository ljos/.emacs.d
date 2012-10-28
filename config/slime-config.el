(setq inferior-lisp-program "sbcl")

(load-file (expand-file-name "~/quicklisp/slime-helper.el"))

(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

(provide 'slime-config)
