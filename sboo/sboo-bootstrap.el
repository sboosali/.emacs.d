;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-bootstrap! ()
 
  "`sboo-bootstrap!' does two things:
  
  * [1] Set package-internal `load-path's,
  via `sboo-bootstrap-register-load-paths',
  for convenient installation of `sboo'.
  
  * [2] Re-export core `feature's, (i.e. just transitively `require' them),
  via `sboo-bootstrap-reexport-features',
  for convenient importing of `sboo'.
  "

  (progn
    (sboo-bootstrap-register-load-paths!)
    (sboo-bootstrap-reexport-features)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-bootstrap-register-load-paths ()
 
  "`'.
  "

  (progn
    (add-to-list 'load-path (expand-file-name "./initialization/"))
    (add-to-list 'load-path (expand-file-name "./configuration/"))
    ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-bootstrap-reexport-features ()
 
  "`'.
  "

  (progn
    (require 'sboo-settings)
    ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-bootstrap)