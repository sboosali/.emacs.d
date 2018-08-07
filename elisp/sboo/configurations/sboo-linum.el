;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `linum`
;;
;; i.e. LIne NUMbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-linum-config ()
  (setq
   display-line-numbers t))
   ;; ^ 
   ;;  automatically becomes buffer-local when set.
   ;;
   ;; `t` or `'relative`.
   ;;
   ;;
   ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; `linum-mode`
;;
;; > The old `linum-mode` is written in elisp, and the performance isn't great. 
;; > The new one is implemented in C, and looks better and runs faster.
;; 


;; See:
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-linum)