;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `subword-mode' and `superword-mode'
;;
;; Move Cursor by `camelCase' and `snake_case'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'use-package)
;; (use-package subword
;;   :hook (haskell-mode)
;;   :config
;;   (defalias 'sub 'subword-mode)
;;   (defalias 'sup 'superword-mode))

;; ^ 
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; e.g.
;;
;; these commands:
;;
;; M-x subword-mode
;; M-: (insert "NSGraphicsContext")
;; M-b M-b M-b
;; 
;; cause these cursors ("|"):
;;
;; NSGraphicsContext|
;; NSGraphics|Context ;; M-b
;; NS|GraphicsContext ;; M-b
;; |NSGraphicsContext ;; M-b
;; 

;; Enabling `subword-mode' redefines "word",
;; so that "word-based" commands stop inside
;; symbols with mixed uppercase and lowercase letters,
;; e.g. "GtkWidget", "EmacsFrameClass", "NSGraphicsContext".
;;
;; Here we call these mixed case symbols ‘nomenclatures’.  Each
;; capitalized (or completely uppercase) part of a nomenclature is
;; called a ‘subword’. Examples:
;;
;;   Nomenclature           Subwords
;;   ===========================================================
;;   GtkWindow          =>  "Gtk" and "Window"
;;   EmacsFrameClass    =>  "Emacs", "Frame" and "Class"
;;   NSGraphicsContext  =>  "NS", "Graphics" and "Context"
;;

;; `subword-mode' and `superword-mode' are (mutually-exclusive) minor-modes.
;;
;; See:
;;     - http://ergoemacs.org/emacs/emacs_subword-mode_superword-mode.html
;;     - C-h f subword-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-subsuperword)