(require 'clojure-mode-autoloads)

(eval-after-load "clojure-mode"
  '(progn
     (put-clojure-indent 'update-in 'defun)
     (put-clojure-indent 'assoc-in 'defun)
     (put-clojure-indent 'swap! 'defun)
     (require 'clojure-jump-to-file)
     (require 'cljdoc)
     (ad-activate 'cljdoc-get-docstring)))


(add-hook 'clojure-mode-hook
          '(lambda () (setq ac-sources
                       (cons ac-source-yasnippet
                             ac-sources))))

(defadvice cljdoc-get-docstring (after truncate-docstring)
  (setq ad-return-value
        (truncate-string-to-width
         (concat " " ad-return-value) (- (frame-width) 10) nil nil 't)))

(provide 'clojure-config)
