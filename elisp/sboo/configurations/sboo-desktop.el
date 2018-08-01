
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DESKTOP
;; 
;; the `desktop` builtin.
;;
;; (Multiple) Desktop "Sessions"
;; 

(progn
  (setq
   desktop-auto-save-timeout 5)
   ;; ^ unit of time is seconds.
  (desktop-save-mode 1)
   ;; ^ (enable a mode after configuring its variables).
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NOTES
;;
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
	
;;;; the `desktop+` package:
;; 
;; (require 'desktop+)
;; (defconst sboo-desktop-name-default
;;  "sboo")
;; (desktop-create sboo-desktop-name-default)
;; (desktop+-load   sboo-desktop-name-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-desktop)