(require 'workgroups)

(setq wg-prefix-key (kbd "C-c w"))
(workgroups-mode 1)

(wg-load "~/.emacs.d/workgroups")

(add-hook 'auto-save-hook 'wg-save-default)
(add-hook 'kill-emacs-hook 'wg-save-default)

(provide 'workgroups-config)
