(eval-after-load 'paredit
  '(progn (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
          (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)))
