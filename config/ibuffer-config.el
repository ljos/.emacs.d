(require 'ibuffer)
(require 'ibuffer-vc)

(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'major-mode)
              (ibuffer-do-sort-by-major-mode))))

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 25 25 :left :elide)
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
