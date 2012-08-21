(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

;;Need to fix some bad keybindings in lisp-starterkit
(eval-after-load 'paredit 
  '(progn
     (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
     (define-key paredit-mode-map (kbd "M-)") 'paredit-close-round-and-newline)
     (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
     (define-key paredit-mode-map (kbd "M-s-{") 'paredit-wrap-curly)))

(provide 'paredit-config)

