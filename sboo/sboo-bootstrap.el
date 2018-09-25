;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;
;; [1] Set package-internal `load-path's,
;; for convenient installation of `sboo'.
;;
;; [2] Re-export core `feature's, (i.e. just `require' them)
;; for convenient importing of `sboo'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-bootstrap! ()
 
  "`'.
  "

  (progn
    (sboo-register-paths!)
    
    ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-register-paths! ()
 
  "`'.
  "

  (progn
    (add-to-list 'load-path (expand-file-name "./initialization/"))
    (add-to-list 'load-path (expand-file-name "./configuration/"))
    ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-bootstrap)