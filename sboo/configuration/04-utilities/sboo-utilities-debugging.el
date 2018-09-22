;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for deubgging (i.e. for `--debug-init').
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; this error:
;;
;;     “Wrong number of arguments: (1 . 2), 0”
;;
;; means that:
;; 
;; * the function expects « at least 1 » argument, and « at most 2 » arguments (i.e. 1 mandatory and 1 optional argument),
;; * but, it was called with « 0 » arguments.
;; 
;; 

;; See:
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-utilities-debugging)