(require 'workgroups)

(setq wg-prefix-key (kbd "C-c w"))
(workgroups-mode 1)

(wg-load "~/.emacs.d/workgroups")

;; Taken from http://mcpantz.org/blog/2012/10/21/moving-from-escreen-to-workgroups/
(defun wg-save-default ()
  "Run `wg-save' on `wg-file'."
  (interactive)
  (when wg-list
    (with-temp-message ""
      (wg-save wg-file))))

(add-hook 'auto-save-hook 'wg-save-default)
(add-hook 'kill-emacs-hook 'wg-save-default)

(provide 'workgroups-config)
