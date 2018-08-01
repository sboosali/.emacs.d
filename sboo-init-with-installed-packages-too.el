;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Configure/initialize several non-builtin packages,
;; which are useful enough I expect to
;; install them immediately and/or ensure they're always present.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defmacro sboo-require-soft (PACKAGE)
;;   "`require` wrapper. 
;;   Doesn't error when the package (i.e. 'feature') is absent.
;;   Returns `t` when the package is present."
;;   (interactive)
;;   (require 'PACKAGE nil t))

;; (defmacro sboo-require-trying (PACKAGE BODY...)
;;   "`require` wrapper. 
;;   Doesn't error when the `PACKAGE` (i.e. a 'feature') is absent.
;;   Executes `BODY` only when the `PACKAGE` is present."
;;   (interactive)
;;   (when (require 'PACKAGE nil t)
;;     (progn
;;       BODY...))

;; sboo-require-then
;; sboo-require-trying

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

(when (require 'shackle nil t)
  (progn
    (require 'sboo-shackle)
    (sboo-config-real-auto-save)))

  ;; ^ 
  ;; 

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