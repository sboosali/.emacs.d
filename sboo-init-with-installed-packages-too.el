;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Configure/initialize several non-builtin packages,
;; which are useful enough I expect to
;; install them immediately and/or ensure they're always present.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'real-auto-save nil t)
  (progn
    (require 'sboo-real-auto-save)
    (sboo-config-real-auto-save)))

;; ^
;; NOTE on the timing of executing these `real-auto-save` settings:
;;
;; While `real-auto-save` is not builtin, it's a short file.
;; The "correct" (IMO) behavior of any auto-save feature is:
;; saving continuously, and not to a different file.
;; It should come early, before opening files (or re-opening them, e.g. via `desktop-save-mode`), to make sure every buffer present (in addition to ones opened later.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'sboo-)
;; (sboo-config-)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'sboo-)
;; (sboo-config-)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'sboo-)
;; (sboo-config-)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to "soft-require" a package, i.e.:
;;
;; - don't error if it's absent;
;; - don't activate / configure it unless it's present.
;;
;; you use:
;;
;;     (when (require '... nil t) ...)
;;
;; e.g.:
;;      (when (require 'projectile nil t)
;;        (do-some-stuff-that-needs-projectile)
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-init-with-installed-packages-too)