(require 'clojure-mode-autoloads)
(require 'cljdoc-autoloads)
(require 'clojure-jump-to-file)

(defadvice cljdoc-get-docstring (after truncate-docstring)
  (setq ad-return-value
        (truncate-string-to-width
         (concat " " ad-return-value) (- (frame-width) 10) nil nil 't)))

(defun clojure-jump-to-project-file ()
  (interactive)
  (let ((dir (file-name-as-directory
              (locate-dominating-file buffer-file-name "src/"))))
    (find-file (concat dir "project.clj"))))

(eval-after-load "clojure-mode"
  '(progn
     (put-clojure-indent 'update-in 'defun)
     (put-clojure-indent 'assoc-in 'defun)
     (put-clojure-indent 'assoc! 'defun)
     (put-clojure-indent 'swap! 'defun)
     (put-clojure-indent 'run* 'defun)
     (put-clojure-indent 'fresh 'defun)
     (ad-activate 'cljdoc-get-docstring)
     (define-key clojure-mode-map (kbd "C-x p") 'clojure-jump-to-project-file)))

(add-hook 'clojure-mode-hook
          '(lambda () (setq ac-sources
                       (cons ac-source-yasnippet
                             ac-sources))))

(add-hook 'clojure-mode-hook
          '(lambda () (paredit-mode +1)))


(provide 'clojure-config)
