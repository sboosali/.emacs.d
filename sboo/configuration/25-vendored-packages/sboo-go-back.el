;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My configuration for the `go-back' package.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-back

  :disabled
  ;; ^ERROR
  ;; Error (use-package): Failed to parse package go-back: Wrong number of arguments: (1 . 1), 2

  :bind (("M-q" . go-back-pop-point-stack)))

;; ^ 
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; « go-back »: An emacs package for revisiting previous positions, undo-style.
;;
;; ## Configuration
;;
;; ```elisp
;; (require 'go-back)
;; (global-set-key (kbd "M-r") #'go-back/pop-point-stack)
;; ```
;;
;; ## Usage
;;
;; call go-back/pop-point-stack repeatedly to jump back through previous positions.
;;
;; NOTE if you perform a sequence of pops, then interrupt it (e.g. with « C-g »), then start popping again, you'll be revisiting the revisits (like emacs' default « undo » behavior).
;;

;; See:
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-go-back)