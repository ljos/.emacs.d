(require 'paredit)


(define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
(define-key paredit-mode-map (kbd "M-)") 'paredit-close-round-and-newline)
(define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
(define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
(define-key paredit-mode-map (kbd "M-}") 'paredit-close-curly-and-newline)

;;for modes that have strings in singlequotes.
(defun paredit-singlequote (&optional n)
  (interactive "P")
  (cond ((paredit-in-string-p)
         (if (eq (cdr (paredit-string-start+end-points))
                 (point))
             (forward-char)             ; We're on the closing quote.
           (insert ?\\ ?\' )))
        ((paredit-in-comment-p)
         (insert ?\' ))
        ((not (paredit-in-char-p))
         (paredit-insert-pair n ?\' ?\' 'paredit-forward-for-quote))))

(provide 'paredit-config)
