(require 'eclim)
(global-eclim-mode)

(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(require 'eclimd)

(provide 'emacseclim-config)
