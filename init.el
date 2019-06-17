;;; init.el --- Personal configuration -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;----------------------------------------------;;
;; Setup ---------------------------------------;;
;;----------------------------------------------;;

(setq load-prefer-newer t)

;; ^ never accidentally `load' outdated (byte-compiled) files.

;;----------------------------------------------;;

(progn
  (package-initialize)
  (setq package-enable-at-startup nil))

;;----------------------------------------------;;

;;(setq debug-on-error t);;TODO environment-variable

;;----------------------------------------------;;

;;(when (boundp 'user-init-file) (find-file user-init-file))
;; ^ for Debugging

;;----------------------------------------------;;
;; Configuration -------------------------------;;
;;----------------------------------------------;;

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
    (with-demoted-errors "[Warning] %s" (find-file SbooFile))
    (load SbooFile nil nil t t)))

;;----------------------------------------------;;
;; Teardown ------------------------------------;;
;;----------------------------------------------;;

(setq debug-on-error nil)

;;; init.el ends here

;;----------------------------------------------;;
;; Customization -------------------------------;;
;;----------------------------------------------;;
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
