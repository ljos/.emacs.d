(provide 'applescript-config)

(add-to-list 'load-path "~/.emacs.d/site-lisp/applescript-mode/")
(autoload 'applescript-mode "applescript-mode"
  "Major mode for editing AppleScript source." t)
(add-to-list 'auto-mode-alist '("\\.applescript$" . applescript-mode))
