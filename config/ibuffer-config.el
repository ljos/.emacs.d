(require 'ibuffer)
(require 'ibuffer-vc)

(setq ibuffer-saved-filter-groups
  (quote (("Default"
           ("Org" ;; all org-related buffers
            (mode . org-mode))
           ("Programming"
            (or
             (mode . c-mode)
             (mode . clojure-mode)
             (mode . emacs-lisp-mode)
             (mode . go-mode)
             (mode . perl-mode)
             (mode . python-mode)
             ))))))

(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-expert t)

(provide 'ibuffer-config)
