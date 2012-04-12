;;Need to fix some bad keybindings in lisp-starterkit
(eval-after-load 'paredit
  '(progn (define-key paredit-mode-map (kbd "M-(") 'paredit-warp-round)
          (define-key paredit-mode-map (kbd "M-)") 'paredit-close-round-and-newline)
          (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
          (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)))
