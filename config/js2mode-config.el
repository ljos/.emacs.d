(require 'js2-mode-autoloads)

(eval-after-load 'js2-mode
  '(progn
     (setq-default js2-auto-indent-p t)
     (setq-default js2-indent-on-enter-key t)
     (setq-default js2-global-externs '("module" "require" "jQuery" "$"
                                        "_" "buster" "sinon" "assert"
                                        "refute" "setTimeout" "clearTimeout"
                                        "setInterval" "clearInterval"
                                        "location" "__dirname" "console" "JSON"))

     (define-key js2-mode-map (kbd ",") 'self-insert-command)

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
                             nil)))))
     (define-key js2-mode-map "{" 'paredit-open-curly)
     (define-key js2-mode-map "}" 'paredit-close-curly-and-newline)
     (add-hook 'js2-mode-hook 'esk-paredit-nonlisp)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(provide 'js2mode-config)
