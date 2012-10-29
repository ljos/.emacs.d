(require 'auto-complete-config)
(ac-config-default)

(setq ac-auto-show-menu 0.3)
(setq ac-use-menu-map t)

(define-key ac-complete-mode-map "\r" 'ac-expand)
(define-key ac-complete-mode-map [return] 'ac-expand)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map [tab] 'ac-complete)

(provide 'autocomplete-config)
