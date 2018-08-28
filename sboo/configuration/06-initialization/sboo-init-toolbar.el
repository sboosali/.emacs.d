;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ToolBar
;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

(require 'sboo-settings-safe)
;; ^ for `ffap-bindings'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :demand t

  :bind
  (("<tool-bar> <new-file>" . find-file-at-point)))
  ;; ^ `find-file-at-point' `ffap-bindings'.

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