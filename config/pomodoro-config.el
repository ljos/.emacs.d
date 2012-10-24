(require 'pomodoro)

(setq pomodoro-break-start-sound "~/Music/ding.mp3")
(setq pomodoro-work-start-sound "~/Music/ding.mp3")
(setq pomodoro-work-start-message "Back to work!")
(setq pomodoro-work-cycle "労働") ;; work in japanese
(setq pomodoro-break-cycle "休止") ;; break in japanese

(global-set-key (kbd "C-c p s") 'pomodoro-start)
(global-set-key (kbd "C-c p x") 'pomodoro-stop)

(provide 'pomodoro-config)
