(require 'pomodoro-autoloads)

(setq pomodoro-mode-line-string "")

(eval-after-load 'pomodoro
  '(progn
     (setq pomodoro-break-start-sound "~/Music/smw_pause.wav")
     (setq pomodoro-work-start-sound "~/Music/smw_pause.wav")
     (setq pomodoro-work-start-message "Back to work!")
     (setq pomodoro-work-cycle "労働") ;; work in japanese
     (setq pomodoro-break-cycle "休止") ;; break in japanese
     (setq pomodoro-long-break-time 20)
     (setq pomodoro-break-time 7)))

(global-set-key (kbd "C-c p s") 'pomodoro-start)
(global-set-key (kbd "C-c p x") 'pomodoro-stop)

(provide 'pomodoro-config)
