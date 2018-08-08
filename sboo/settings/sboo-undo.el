;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; 
;; all external undo packages have corruption problems.

;; the "internal" undo behavior is:
;; - To redo, just Press Ctrl+g first then undo. further undo will be redo. 
;; - Press Ctrl+g again to reverse direction. ("If you are careful, one can avoid the undo/redo roller-coaster confusion.")

;; See
;; - http://ergoemacs.org/emacs/emacs_best_redo_mode.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-undo)