(remove-hook 'clojure-mode-hook 'esk-pretty-fn)
(add-hook 'clojure-mode-hook
          '(lambda ()
             (put-clojure-indent 'update-in 'defun)))
