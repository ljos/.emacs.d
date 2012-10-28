(require 'clojure-mode-autoloads)

(eval-after-load "clojure-mode"
  '(progn
     (put-clojure-indent 'update-in 'defun)
     (put-clojure-indent 'assoc-in 'defun)  
     (put-clojure-indent 'swap! 'defun)))

(provide 'clojure-config)
