;;; init.el --- Personal configuration -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;----------------------------------------------;;
;; Setup ---------------------------------------;;
;;----------------------------------------------;;

(setq debug-on-error t)

(setq load-prefer-newer t)

;; ^ never accidentally `load' outdated (byte-compiled) files.

;;----------------------------------------------;;
;; Configuration -------------------------------;;
;;----------------------------------------------;;

(setq custom-file "~/.emacs.d/sboo/sboo-custom.el")

;; ^ redirect custom file early (for `customize') .

(progn
  (setq sboo-init-file "~/.emacs.d/sboo/sboo-init.el")
  (load sboo-init-file nil nil t t)
  )

;;(call-interactively #'server-start)

;;----------------------------------------------;;
;; Teardown ------------------------------------;;
;;----------------------------------------------;;

(setq debug-on-error nil)

;;; init.el ends here

;;----------------------------------------------;;
;; Customization -------------------------------;;
;;----------------------------------------------;;
