(add-to-list 'load-path "~/.emacs.d/site-lisp/arduino-mode")

(autoload 'arduino-mode  "arduino-mode.el"
  "Major mode for editing SPARQL files" t)

(add-hook 'arduino-mode-hook '(lambda () (idle-highlight-mode +1)))

(add-to-list 'auto-mode-alist '("\\.ino$" . arduino-mode))

(provide 'arduino-config)
