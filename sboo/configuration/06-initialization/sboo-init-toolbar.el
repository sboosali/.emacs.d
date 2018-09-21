;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My `tool-bar' Configuration.
;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-settings-safe)
;; ^
;; for `ffap-bindings'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (global-set-key (kbd "<tool-bar> <new-file>") #'find-file-at-point)
  ;; ^
  ;; `find-file-at-point' `ffap-bindings'.

  t)

;; ^ "<tool-bar> <new-file>" crashes my system;
;; i.e. both KDE and GNOME crash; neither killing emacs nor restarting them works.
;;
;;TODO `find-file-at-point'
;;TODO `helm-find-files'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; See:
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-init-toolbar)