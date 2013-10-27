(add-to-list 'load-path "~/.emacs.d/site-lisp/sparql-mode")

(autoload 'sparql-mode  "sparql-mode.el"
  "Major mode for editing SPARQL files" t)

(add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))

(eval-after-load "sparql-mode"
  '(progn
     (add-to-list 'ac-dictionary-files "~/.emacs.d/site-lisp/sparql-mode/sparql-mode")
     (setq sparql-default-base-url "http://live.dbpedia.org/sparql")))

(add-hook 'sparql-mode-hook 'auto-complete-mode)

(add-hook 'sparql-result-mode-hook '(lambda () (linum-mode -1)))
(add-hook 'sparql-result-mode-hook '(lambda () (toggle-truncate-lines 1)))

(provide 'sparql-config)
