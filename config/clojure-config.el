;;removing pretty-fn that lisp-starterkit introduces
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)

(eval-after-load 'clojure-mode-hook
  '(progn (put-clojure-indent 'update-in 'defun)))

(provide 'clojure-config)
