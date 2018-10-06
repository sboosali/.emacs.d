;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports (Bootstrapped) ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((EmacsDirectory     (or user-emacs-directory "~/.emacs.d/"))
       (SbooDirectory      (file-name-as-directory (concat EmacsDirectory "sboo/")))
       (SbooFile           (file-truename (concat SbooDirectory "sboo-definitions.el"))))   ;;TODO EnvironmentVars

  (require 'sboo-definitions SbooFile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-to-load-path! (FilePath)

  "Register `FilePath' with `load-path'.

  `FilePath':
  
  * /must/ be an absolute filepath to a directory; (TODO)
  
  * /should/ use forward-slashes, e.g. `.../.../...'
    (they're automatically converted to the platform-specifc directory-separator character);
  
  * /may/ start with `~/' 
    (tildes are expanded to the user's home directory);

  * /may/ end with a forward-slash (e.g. `sboo/' or `sboo')
    (a trailing is added if absent).
  "

  (add-to-list 'load-path
     (file-name-as-directory (file-truename FilePath))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register LoadPaths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-load-path! sboo-root-directory)
(add-to-load-path! sboo-lisp-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sboo-load-file! "sboo-settings.el")
(sboo-load-file! "sboo-aliases.el")
(sboo-load-file! "sboo-commands.el")
(sboo-load-file! "sboo-keybindings.el")

(when (require 'sboo-server nil t)
  (add-hook 'after-init-hook #'server-start-unless-running))

;;;(require 'sboo-settings-widgets)
;;;  (sboo-minibuffer-config))
;;;  (sboo-config-fonts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages: Installation ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sboo-require! 'sboo-packages-by-installing)
;;;(sboo-require! 'sboo-packages-by-vendoring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages: Configuration ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sboo-require! 'sboo-init-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(eval-when-compile (require 'use-package))
;;(setq use-package-verbose t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-init)