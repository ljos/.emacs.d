(require 'clojure-mode-autoloads)
(require 'midje-mode-autoloads)

(eval-after-load "clojure-mode"
  '(progn
     (put-clojure-indent 'update-in 'defun)
     (put-clojure-indent 'assoc-in 'defun)  
     (put-clojure-indent 'swap! 'defun)
     (require 'clojure-jump-to-file)
     (require 'cljdoc)


(add-hook 'clojure-mode-hook
          '(lambda () (setq ac-sources
                       (cons ac-source-yasnippet
                             ac-sources))))

(provide 'clojure-config)
