(require 'haskell-mode-autoloads)

(eval-after-load "haskell-mode"
  '(progn
     (add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
     (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)))
