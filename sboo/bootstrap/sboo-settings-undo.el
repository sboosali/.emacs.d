;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure the Behavior of `undo'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  
  (setq undo-limit        20000000)
  (setq undo-strong-limit 40000000)
  
  ;; ^ ensure undo limits are as high as possible.

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the "internal" undo behavior is:
;;
;; - To redo, just Press Ctrl+g first then undo. further undo will be redo. 
;; - Press Ctrl+g again to reverse direction. ("If you are careful, one can avoid the undo/redo roller-coaster confusion.")

;; > all external undo-packages have corruption problems.

;; See
;; - http://ergoemacs.org/emacs/emacs_best_redo_mode.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-settings-undo)