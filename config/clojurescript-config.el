(require 'clojure-mode-autoloads)

(define-derived-mode clojurescript-mode clojure-mode "ClojureScript"
  "Major mode for ClojureScript")

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))

(provide 'clojurescript-config)
