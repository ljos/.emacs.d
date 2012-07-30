;;; Modified from http://stackoverflow.com/questions/11484225/fix-an-auto-complete-mode-and-linum-mode-annoyance/11496199#11496199
(provide 'linum-config)

(defvar my-linum-format-string "%4d")

(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

(defun my-linum-get-format-string ()
  (let* ((width (length (number-to-string
                         (count-lines (point-min) (point-max)))))
         (format (concat "%" (number-to-string width) "d ")))
    (setq my-linum-format-string format)))

(setq linum-format 'my-linum-format)

(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face 'linum))
