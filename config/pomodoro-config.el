(require 'pomodoro-autoloads)

(defvar pomodoro-time-spent (make-hash-table :test 'equal))

(defmacro frontmost-p ()
  (cond ((eq 'darwin system-type)
         `(string= "true\n"
                   (shell-command-to-string
                    "osascript -e \"tell application \\\"Emacs\\\" to get frontmost\"")))
        ((eq 'x window-system)
         (let* ((active-window (x-window-property
                                "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t))
                (active-window-id (if (numberp active-window)
                                      active-window
                                    (string-to-number
                                     (format "%x00%x"
                                             (car active-window)
                                             (cdr active-window)) 16)))
                (emacs-window-id (string-to-number
                                  (frame-parameter nil 'outer-window-id))))
           (= emacs-window-id active-window-id)))))

(defadvice pomodoro-tick (after pomodoro-check-time-spent)
  (if (string= pomodoro-work-cycle pomodoro-current-cycle)
      (if (frontmost-p)
          (let ((k (or buffer-file-truename (buffer-name))))
            (puthash k (1+ (gethash k pomodoro-time-spent 0)) pomodoro-time-spent))
        (puthash "nil" (1+ (gethash "nil" pomodoro-time-spent 0)) pomodoro-time-spent))))

;; we need to set this out here as it is used in the modeline
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
