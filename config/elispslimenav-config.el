(require 'elisp-slime-nav)

(add-hook 'emacs-lisp-mode-hook
          (lambda () (elisp-slime-nav-mode t)))

(provide 'elispslimenav-config)
