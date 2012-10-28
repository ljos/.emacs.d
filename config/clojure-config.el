(require 'clojure-mode)

(put-clojure-indent 'update-in 'defun)
(put-clojure-indent 'assoc-in 'defun)  
(put-clojure-indent 'swap! 'defun)

(provide 'clojure-config)
