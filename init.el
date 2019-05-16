;;; init.el --- SBoo's Emacs Configuration -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

(setq package-enable-at-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(setq debug-on-error t);;TODO environment-variable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(when (boundp 'user-init-file) (find-file user-init-file))
;; ^ for Debugging

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((EmacsDirectory (or user-emacs-directory
			   "~/.emacs.d/"))
       (SbooDirectory  (file-name-as-directory (concat EmacsDirectory
						       "sboo/")))
       (SbooCustom     (file-truename (concat SbooDirectory
					      "sboo-custom.el")))
       (SbooFile       (file-truename (concat SbooDirectory
					      "sboo-init.el"))))   ;;TODO EnvironmentVars

  (progn
    (setq custom-file SbooCustom)       ; redirect custom file early.
    (ignore-errors (find-file SbooFile))
    (load SbooFile nil nil t t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-error nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
