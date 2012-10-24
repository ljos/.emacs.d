(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-directory "~/Dropbox/org")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/inbox.org")
(setq org-mobile-directory "~/Dropbox/org/mobile")

(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)

(setq org-agenda-files '("~/Dropbox/org/organizer.org"))

(setq org-tag-persistent-alist
      '((:startgroup . nil)
        ("work" . ?o) ("home" . ?h)
        (:endgroup . nil)
        ("computer" . ?c) ("project" . ?p) ("reading" . ?r)
        ("watch" . ?d) ("errand" . ?l)))

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING"
                  "|" "DONE" "CANCELLED" "DEFERRED" "DELEGATED")
        (sequence "APPT" "|" "FINISHED" "CANCELLED" "MISSED")
        (sequence "BUG" "|" "FIXED")))

(setq org-refile-targets '(("organizer.org" :maxlevel . 9)))
(setq org-completion-use-ido t)

(require 'org-latex)

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))  

(setq org-latex-to-pdf-process '("texi2dvi --pdf --verbose --batch %f"))

(add-to-list 'org-export-latex-classes
             `("thesis"
               "\\documentclass{report}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(require 'org-special-blocks)

(add-hook 'org-mode-hook (lambda () (visual-line-mode t)))
(add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))
(add-hook 'org-mode-hook
          (lambda ()
            (font-lock-remove-keywords
             nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
                    1 font-lock-warning-face t)))))




(require 'remember)

(setq org-remember-templates
      '(("Tasks" ?t "* TODO %?\n  %i\n  %a"
         "~/Dropbox/org-mode/organizer.org" "TASKS")
        ("Appointments" ?a "* APPT %?\nSCHEDULED: %^T\n%i\n  %a"
         "~/Dropbox/org-mode/organizer.org" "APPOINTMENTS")
        ("New project" ?p "* %?" "~/Dropbox/org-mode/organizer.org" "PROJECTS")))

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))

(eval-after-load 'remember
  '(add-hook 'remember-mode-hook 'org-remember-apply-template))

(global-set-key (kbd "\C-cr") 'remember)  

(provide 'org-config)
