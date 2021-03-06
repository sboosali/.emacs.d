;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wdired
;;
;; i.e. Writable DIRectory EDitor
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-dired-initialize ()
  
  (setq
   dired-isearch-filenames 'dwim
   dired-dwim-target       t
   dired-omit-extension    '(".hi" ".p_hi" ".o" ".a")
   dired-listing-switches  "-alh")
  ;; ^

  nil)

  ;;TODO
  ;; (add-hook 'dired-mode-hook (lambda ()
  ;;  (define-key dired-mode-map
  ;;    "z" 'dired-zip-files)
  ;;  (define-key dired-mode-map 
  ;;   (kbd "RET") 'dired-find-alternate-file)
  ;;  (define-key dired-mode-map 
  ;;   (kbd "w") 'wdired-change-to-wdired-mode)
  ;;  (dired-omit-mode 1))))
  ;; (add-hook 'dired-mode-hook (lambda ()
  ;;                              (dired-omit-mode 1)))
  ;; ;; ^ uses `dired-omit-extension' (above).


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
 
(use-package dired
  :init
  (sboo-dired-initialize))

(use-package wdired
  :init
  (sboo-wdired-initialize))

  ;; ^ `dired' and `wdired'.
  ;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; KeyBindings
;; 
;; dired is read-only, by default. switch to writable with the (globally-recognized) keybinding:
;;   C-x C-q
;;
;; commit changes with the (standard) keybinding:
;;   C-c C-c
;;
;; cancel changes with the keybinding:
;;   C-c ESC
;; 
;; list all files recursively, in the `dired' buffer.
;;   « C-u s R RET » 
;;

;; Notes
;;
;; « C-u s R RET »
;; i.e. run `ls', adding the `-R' switch (a.k.a. "recursive").
;;
;; 
;; 

;; See:
;;     - https://www.masteringemacs.org/article/wdired-editable-dired-buffers
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-wdired)