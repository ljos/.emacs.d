;;; init.el -- Initializes emacs by loading the org babel files
(require 'package)
(when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)

(use-package org
  :ensure t
  :init
  (org-babel-load-file
   (expand-file-name "configuration.org" user-emacs-directory)))

;;; init.el ends here
