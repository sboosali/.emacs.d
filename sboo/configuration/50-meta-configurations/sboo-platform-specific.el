;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-platform

  (cond

   ((or (memq system-type   '(gnu/linux))
        (memq window-system '(x)))
         ;; ^ TODO Wayland?
    'sboo-linux)

   ((or (memq system-type   '(windows-nt ms-dos))
        (memq window-system '(w32 pc)))
    'sboo-windnows)

   ((or (memq system-type   '(darwin))
        (memq window-system '(mac ns)))

    'sboo-macintosh))

  "The `feature' (i.e. Emacs module) `load'ed for the current platform.
   Has type Symbol (the symbols are named after the different supported operating-systems).
   In particular, one of: '`sboo-linux',  '`sboo-windows', '`sboo-macintosh'.
   Uses the `system-type' and `window-system' variables.")

;; TODO? `case'
;; (case window-system
;;  ('mac (require 'sboo-macintosh)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require sboo-platform nil t)
;; ^ a "soft-require".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes

;; ^ `windows-system':
;;
;; Emacs is displaying the frame using X. 
;; ---
;; w32
;; Emacs is displaying the frame using native MS-Windows GUI. 
;; ---
;; pc
;; Emacs is displaying the frame using MS-DOS direct screen writes. 
;; ---
;; ns
;; Emacs is displaying the frame using the Nextstep interface (used on GNUstep and macOS). 
;; ---
;; nil
;; Emacs is displaying the frame on a character-based terminal.
;;
;; See
;;     - https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Systems.html
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-platform-specific)