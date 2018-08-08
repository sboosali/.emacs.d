;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Purpose: Framework For Categorizing Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See:
;; 
;; - https://github.com/bmag/emacs-purpose/blob/master/README.md
;;
;; -
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(use-package window-purpose

  :config
  (purpose-mode 1)

  

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; e.g.
;; ;;
;; (require 'window-purpose)
;; (purpose-mode)
;; ;;
;; ;; c-programming purpose configuration:
;; ;;
;; (add-to-list 'purpose-user-mode-purposes '(c-mode   . c-file-purpose))
;; (add-to-list 'purpose-user-mode-purposes '(asm-mode . c-file-purpose))
;; (add-to-list 'purpose-user-mode-purposes '(c++-mode . c-file-purpose))
;; (add-to-list 'purpose-user-mode-purposes '(cscope-list-entry-mode . cscope-purpose))
;; (add-to-list 'purpose-user-mode-purposes '(compilation-mode . compilation-purpose))
;; ;;
;; ;; build it
;; (purpose-compile-user-configuration)
;; ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-purpose)