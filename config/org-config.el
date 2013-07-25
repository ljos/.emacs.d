(require 'org-autoloads)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-directory "~/Dropbox/org")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/inbox.org")
(setq org-mobile-directory "~/Dropbox/org/mobile")

(setq org-agenda-include-all-todo t)

(setq org-agenda-files '("~/Dropbox/org/organizer.org"))

(setq org-tag-persistent-alist
      '(("work" . ?w) ("private" . ?p)))

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING"
                  "|" "DONE" "CANCELLED" "DEFERRED" "DELEGATED")
        (sequence "APPT" "|" "FINISHED" "CANCELLED" "MISSED")
        (sequence "BUG" "|" "FIXED")))

(setq org-todo-keyword-faces
      '(("STARTED" . "yellow")
        ("CANCELLED" . "dim gray")))

(setq org-refile-targets '(("organizer.org" :maxlevel . 9)))
(setq org-completion-use-ido t)
(setq org-latex-to-pdf-process '("texi2dvi --pdf --verbose --batch %f"))

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass{article}
                \\usepackage[l2tabu, orthodox]{nag}
                \\usepackage{microtype}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-export-latex-classes
             `("thesis"
               "\\documentclass{report}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-hook 'org-mode-hook
          (lambda ()
            (progn
              (define-key org-mode-map (kbd "M-q") 'org-fill-paragraph)
              (visual-line-mode t)
              (setq fill-column 80)
              (setq ispell-parser 'tex)
              (font-lock-remove-keywords
               nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)\\b"
                      1 font-lock-warning-face t))))))

;;org-capture config
(setq org-default-notes-file (concat org-directory "/organizer.org"))
(global-set-key "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("a" "Appointments" entry
         (file+headline org-default-notes-file "Appointments")
         "* APPT %? %^{WITH}p %^{LOCATION}p\n%^T--%^T\n" :prepend)
        ("p" "Project" entry
         (file+headline org-default-notes-file "Projects")
         "* %?\n")
        ("d" "Done" entry
         (file+datetree (concat org-directory "/done.org"))
         "* %?\nCLOCK: %^U--%U")
        ("j" "Journal" entry
         (file+datetree (concat org-directory "/journal.org"))
         "* %?\nEntered on %U\n  %i\n  %a")
        ("t" "Todo" entry
         (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %i\n  %a")))

(defun org-export-latex-no-toc (depth)
  (when depth
    (format "%% Org-mode is exporting headings to %s levels.\n"
            depth)))
(setq org-export-latex-format-toc-function 'org-export-latex-no-toc)

(provide 'org-config)
