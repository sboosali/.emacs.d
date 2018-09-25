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
    (sboo-bootstrap-register-load-paths)
    (sboo-bootstrap-reexport-features)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-bootstrap-register-load-paths ()
 
  "`'.
  "

  (progn
    (add-to-list 'load-path (expand-file-name "./installation/"))
    
    (add-to-list 'load-path (expand-file-name "./initialization/"))
    
    (add-to-list 'load-path (expand-file-name "./configuration/"))
    (add-to-list 'load-path (expand-file-name "./configuration/02-platforms/"))
    (add-to-list 'load-path (expand-file-name "./configuration/03-window-systems/"))
    (add-to-list 'load-path (expand-file-name "./configuration/04-utilities/"))
    (add-to-list 'load-path (expand-file-name "./configuration/05-keybindings/"))
    (add-to-list 'load-path (expand-file-name "./configuration/06-initialization/"))
    (add-to-list 'load-path (expand-file-name "./configuration/07-settings/"))
    (add-to-list 'load-path (expand-file-name "./configuration/10-internal-packages/"))
    ;;TODO;;(add-to-list 'load-path (expand-file-name "./configuration/25-vendored-packages/"))
    (add-to-list 'load-path (expand-file-name "./configuration/30-external-packages/"))
    (add-to-list 'load-path (expand-file-name "./configuration/35-external-configurations/"))
    (add-to-list 'load-path (expand-file-name "./configuration/50-meta-configurations/"))
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