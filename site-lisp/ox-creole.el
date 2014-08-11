;;; ox-oddmuse.el --- Oddmuse Back-End for Org Export Engine

;; Copyright (C) 2014 Bjarte Johansen

;; Author: Bjarte Johansen <bjarte.johansen at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library implements an Oddmuse back-end for Org generic exporter.
;; See Org manual for more information.

;;; Code:

(require 'ox)
(require 'ox-publish)

(org-export-define-backend 'creole
  '((code . org-creole-verbatim)
    (headline . org-creole-headline)
    (item . org-creole-item)
    (link . org-creole-link)
    (paragraph . org-creole-paragraph)
    (plain-text . org-creole-plain-text)
    (plain-list . org-creole-plain-list)
    (section . org-creole-section)
    (src-block . org-creole-verbatim)))

(defun org-creole-verbatim (verbatim contents info)
  (concat "{{{\n"
          (org-element-property :value verbatim)
          "}}}"))

(defun org-creole-item (item contents info)
  (format "%s %s"
          (if (eq 'ordered (org-element-property :type (org-export-get-parent item)))
              "#" "*")
          contents))

(defun org-creole-link (link contents info)
  (format "[[%s|%s]]"
          (org-element-property :raw-link link)
          contents))

(defun org-creole-paragraph (paragraph contents info)
  contents)

(defun org-creole-plain-text (text info)
  text)

(defun org-creole-plain-list (list contents info)
  contents)

(defun org-creole-section (section contents info)
  contents)

(defun org-creole-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Oddmuse."
  (let* ((level (org-export-get-relative-level headline info))
         (title (org-export-data (org-element-property :title headline) info))
         (header (make-string (1+ level) ?=)))
    (concat header " " title " " header "\n"
            contents)))

(defun org-creole-export-as-markdown (&optional async subtreep visible-only)
  (interactive)
  (org-export-to-buffer 'creole "*Org Creole Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

(defun org-creole-publish-to-creole (&optional asyc subtreep visible-only)
  (interactive)
  (let ((outfile (org-export-output-file-name ".creole" subtreep)))
    (org-export-to-file 'creole outfile async subtreep visible-only)))
