;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AutoSaving & BackingUp ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq backup-by-copying t)

;; ^
;; 
;; See:
;;     - https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;;

;;TODO auto-save-interval nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-save-file-name-transforms
 `((".*" ,temporary-file-directory t)))

;; (setq backup-directory-alist
;;    `((".*" . ,temporary-file-directory)))

 ;; ^ store all backup and autosave files in the system temporary directory, and not in the current folder.
 ;;
 ;; `auto-save-mode` auto-saves a file every few seconds and/or every few characters.
 ;;
 ;; `temporary-file-directory` equals `"/tmp/"`, by default.
 ;;
 ;; See:
 ;;     http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/
 ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-autosave)