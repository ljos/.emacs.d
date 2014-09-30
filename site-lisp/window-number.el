;;; window-number.el --- Select windows by numbers.

;; Filename: window-number.el
;; Description: Select windows by numbers.
;; Author: Johann "Myrkraverk" Oskarsson <myrkraverk@users.sourceforge.net>
;; Maintainer: Johann "Myrkraverk" Oskarsson <myrkraverk@users.sourceforge.net>
;;             Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2004, Johann "Myrkraverk" Oskarsson, all rights reserved.
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Copyright (C) 2014, Bjarte Johansen, all rights reserved.
;; Created: 2004
;; Version: 0.2
;; Last-Updated: 2014-09-20 15:38
;;           By: Bjarte Johansen
;; URL: http://www.emacswiki.org/emacs/download/window-number.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;; `cl'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Window number mode allows you to select windows by numbers.
;;
;; `window-number-switch' same as `other-window' (C-x o) when windows less than three.
;; If have three windows (or more) showing, `window-number-switch' will
;; highlight window number at mode-line then prompt you input window number.
;;
;; I binding `window-number-switch' on 'C-x o' to instead `other-window'.
;;

;;; Installation:
;;
;; Put window-number.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'window-number)
;; (window-number-mode 1)
;;
;; No need more.

;;; Customize:
;;
;; `window-number-active-foreground'
;; `window-number-active-background'
;; `window-number-inactive-foreground'
;; `window-number-inactive-background'
;; `window-number-keymap-prefix'
;;
;; All of the above can customize by:
;;      M-x customize-group RET window-number RET
;;

;;; Change log:
;; 2014/09/20
;;      * Add new variable `window-number-keymap-prefix'.
;;      * Add new variable `window-number-command-map' as base map for numbered commands.
;;      * Define `window-number-mode-map' in terms of `window-number-keymap-prefix' and `window-number-command-map'.
;;      * Make `window-number-meta-mode' obsolete.
;;      * Make `window-number-define-keys' obsolete.
;;
;; 2014/01/24
;;      * Fixed bug of `window-number-set-active-color' and `window-number-set-inactive-color' that not use user's color.
;;
;; 2014/01/01
;;      * Add new function `window-number-switch'
;;      * Add group `window-number' and customize colors.
;;      * Highlight window number on mode-line when `window-number-switch' prompt input.
;;      * Use `completing-read' instead `read-string' for better input experience.
;;
;; 2004
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require

(require 'cl-lib) ; for set-difference and loop

;;; Code:

(defgroup window-number nil
  "Window number group")


(defcustom window-number-active-foreground "black"
  "The foreground color when window number active."
  :type 'string
  :group 'window-number)

(defcustom window-number-active-background "gold"
  "The background color when window number active."
  :type 'string
  :group 'window-number)

(defcustom window-number-inactive-foreground "white"
  "The foreground color when window number inactive."
  :type 'string
  :group 'window-number)

(defcustom window-number-inactive-background "darkred"
  "The background color when window number inactive."
  :type 'string
  :group 'window-number)

(defface window-number-face nil
  "The face used for the window number in the mode-line.")

(defun window-number-list ()
  "Returns a list of the windows, in fixed order and the
minibuffer (even if not active) last."
  (let* ((walk-windows-start
          (car (set-difference
                (window-list (selected-frame) t)
                (window-list (selected-frame) 1))))
         (walk-windows-current walk-windows-start)
         list)
    (while (progn
             (setq walk-windows-current
                   (next-window walk-windows-current t))
             (setq list (cons walk-windows-current list))
             (not (eq walk-windows-current walk-windows-start))))
    (reverse (cons (car list) (cdr list)))))

(defun window-number-switch ()
  "Call `other-window' when just two windows.
Prompt user input window number if have more windows."
  (interactive)
  (if (< (length (window-list)) 3)
      (call-interactively 'other-window)
    (window-number-set-active-color)
    (unwind-protect
        (let* ((window-numbers (number-sequence 1 (length (window-list))))
               (window-buffer-names (mapcar (lambda (x) (buffer-name (window-buffer x))) (window-number-list)))
               (completing-list (mapcar (lambda (x) (list (concat (number-to-string x) " <" (elt window-buffer-names (- x 1)) ">") x)) window-numbers))
               (current-window-index (1+ (position (selected-window) (window-number-list))))
               (next-window-index (if (= current-window-index (length (window-list))) 1 (+ current-window-index 1)))
               (select-index-string (completing-read (format "Window number (%s): " next-window-index) completing-list))
               )
          (window-number-select
           (if (string= select-index-string "")
               next-window-index
             (string-to-int select-index-string))))
      ;; Reset to inactive color if interactive is intercept by Ctrl+g
      (window-number-set-inactive-color)
      ))
  ;; Always reset to inactive color at end.
  (window-number-set-inactive-color))

(defun window-number-select (number)
  "Selects the nth window."
  (interactive "P")
  (if (integerp number)
      (let ((window (nth (1- number) (window-number-list))))
        (if (and window (or (not (window-minibuffer-p window))
                            (minibuffer-window-active-p window)))
            (select-window window)
          (error "No such window.")))))

(defun window-number-set-inactive-color ()
  (set-face-foreground 'window-number-face window-number-inactive-foreground)
  (set-face-background 'window-number-face window-number-inactive-background)
  (force-mode-line-update))

(defun window-number-set-active-color ()
  (set-face-foreground 'window-number-face window-number-active-foreground)
  (set-face-background 'window-number-face window-number-active-background)
  (force-mode-line-update))

(defun window-number ()
  "Returns the the number of the current window."
  (length
   (memq (selected-window)
         (nreverse (window-number-list)))))

(defun window-number-string ()
  "Returns the string containing the number of the current window"
  (propertize
   (concat " [" (number-to-string (window-number)) "] ")
   'face
   'window-number-face))

(defvar window-number-mode-map
  (let ((map (make-sparse-keymap)))
    (cl-loop for number from 1 to 10 collect
             (define-key map
               (kbd (concat "s-" (number-to-string
                                  (if (= number 10) 0 number))))
               `(lambda () (interactive)
                  (window-number-select ,number))))
    map)
  "Keymap for window-number commands after `window-number-keymap-prefix'")

(define-minor-mode window-number-mode
  "A global minor mode that enables selection of windows
according to numbers with the `window-number-keymap-prefix'."
  :global t
  :init-value nil
  :lighter (:eval (window-number-string))
  (window-number-set-inactive-color))

(provide 'window-number)

;;; window-number.el ends here
