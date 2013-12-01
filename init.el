;;; init.el -- Initializes emacs by loading the org babel files
(require 'package)

(package-initialize)

(require 'org-install)
(require 'ob-tangle)
;; Load the org file
(org-babel-load-file
 (expand-file-name "configuration.org" user-emacs-directory))

;;; init.el ends here
