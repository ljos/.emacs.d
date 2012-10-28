(require 'elisp-slime-nav-autoloads)

(add-hook 'emacs-lisp-mode-hook
          (lambda () (elisp-slime-nav-mode t)))

(provide 'elispslimenav-config)
