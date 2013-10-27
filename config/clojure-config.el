(require 'clojure-mode-autoloads)
(require 'cljdoc-autoloads)
(require 'clojure-jump-to-file)

(defadvice cljdoc-get-docstring (after truncate-docstring)
  (setq ad-return-value
        (truncate-string-to-width
         (concat " " ad-return-value) (- (frame-width) 10) nil nil 't)))

(eval-after-load "clojure-mode"
  '(progn
     (put-clojure-indent 'update-in 'defun)
     (put-clojure-indent 'assoc-in 'defun)
     (put-clojure-indent 'assoc! 'defun)
     (put-clojure-indent 'swap! 'defun)
     (put-clojure-indent 'run* 'defun)
     (put-clojure-indent 'fresh 'defun)
     (require 'cljdoc)
     (ad-activate 'cljdoc-get-docstring)))


(add-hook 'clojure-mode-hook
          '(lambda () (setq ac-sources
                       (cons ac-source-yasnippet
                             ac-sources))))

(add-hook 'clojure-mode-hook
          '(lambda () (paredit-mode +1)))


(provide 'clojure-config)
