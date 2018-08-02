;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DESKTOP
;; 
;; the `desktop` builtin-package.
;;
;; (Multiple) Desktop "Sessions"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package emacs ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-desktop-directory
  (if (boundp 'sboo-emacs-directory)
      sboo-emacs-directory
    default-directory))

;;TODO
;; $ mkdir -p "~/.emacs.d/persisted/desktop/.emacs.desktop"
;; (concat sboo-emacs-directory "/persisted/desktop")
;; "/.emacs.desktop"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-config-desktop ()
  "
  "
  (interactive)

  (setq desktop-auto-save-timeout 5)
  ;; ^ unit of time is seconds.
  
  (desktop-save-mode 1)
  ;; ^ (enable a mode after configuring its variables).

  (desktop-read sboo-desktop-directory)
  ;; ^
)

;; TODO auto-save this every so-often (or on  buffer create/delete change events).
; (desktop-save sboo-desktop-directory)
; ;; ^

;; TODO `desktop-save` on emacs exit (i.e. `C-x C-c`).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `desktop-path`
;; default value is `("~/.emacs.d/" "~")`
;;
;; > List of directories to search for the desktop file.
;; > The base name of the file is specified in ‘desktop-base-file-name’.
;;

;; `desktop-base-file-name is a variable defined in ‘desktop.el’.
;; Its value is ".emacs.desktop"
;; Name of file for Emacs desktop, excluding the directory part.


;; relevant `desktop-*` functions and variables:
;;
;; desktop-read
;; desktop-save
;; desktop-save-mode
;; desktop-save-hook
;; desktop-enable 
;; desktop-base-file-name
;; desktop-auto-save-timeout
;; desktop-auto-save-timer
;; desktop-buffers-not-to-save
;; desktop-files-not-to-save
;; desktop-modes-not-to-save
;; desktop-path
;; desktop-restore-eager
;; desktop-restore-frames
;; desktop-load-locked-desktop
;;
;; NOTE
;; 
;; To manually interact with your desktop session at any time, use:
;; - the command ‘M-x desktop-save’ to save it.
;; - the command ‘M-x desktop-read’ to load it.
;;
;; 
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the `desktop+` package:
;; 
;; (require 'desktop+)
;; (defconst sboo-desktop-name-default
;;  "sboo")
;; (desktop-create sboo-desktop-name-default)
;; (desktop+-load   sboo-desktop-name-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the `desktop` lock-file holds the PID of the Emacs process that called `desktop-save` on the lock-file's directory.
;;
;; e.g.
;; 
;; $ cat ~/.emacs.d/.emacs.desktop.lock
;; No such file or directory
;; 
;; M-x desktop-save
;; 
;; $ cat ~/.emacs.d/.emacs.desktop.lock
;; 31194
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `desktop-save`
;;
;; (desktop-save DIRNAME &optional RELEASE)
;;
;; Save the desktop in a desktop file.
;;
;; Parameter DIRNAME specifies where to save the desktop file.
;; Optional parameter RELEASE says whether we’re done with this desktop. 
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-desktop)