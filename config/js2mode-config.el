(require 'js2-mode-autoloads)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(eval-after-load "js2-mode"
  (progn
    ;; Use lambda for anonymous functions
    (font-lock-add-keywords
     'js2-mode `(("\\(function\\) *("
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1) "\u0192")
                            nil)))))

    ;; Use right arrow for return in one-line functions
    (font-lock-add-keywords
     'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1) "\u2190")
                            nil)))))))

(provide 'js2mode-config)
