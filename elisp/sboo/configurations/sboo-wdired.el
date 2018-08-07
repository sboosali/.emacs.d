;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wdired
;;
;; i.e. Writable DIRectory EDitor
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-wdired-initialize ()
  (setq

   wdired-allow-to-change-permissions t
   ;; ^ edit the permission bits directly.
   ;; wdired validates the permission-bits.
  
   wdired-allow-to-redirect-links     t
   ;; ^ rewrite symlinks. 
   ;; (already enabled, by default)
  
   wdired-use-interactive-rename      t
   ;; ^ prompts for confirmation for each filename change you have made (when you commit the changes with C-c C-c).
  
   wdired-confirm-overwrite           t
   ;; ^ prompts for confirmation to overwrite;
   ;; i.e. if your altered filenames conflict with existing files.
   ))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(use-package wdired
  :init
  (sboo-wdired-initialize))

  ;; ^ 
  ;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; [keybindings]
;; 
;; dired is read-only, by default. switch to writable with the (globally-recognized) keybinding:
;;   C-x C-q
;;
;; commit changes with the (standard) keybinding:
;;   C-c C-c
;;
;; cancel changes with the keybinding:
;;   C-c ESC

;; See:
;;     - https://www.masteringemacs.org/article/wdired-editable-dired-buffers
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-wdired)