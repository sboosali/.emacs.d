;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SHACKLE: LAYOUT / PINNING OF WINDOWS ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See:
;;
;;     - https://github.com/wasamasa/shackle#readme
;;
;;     - https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-shackle.el
;;
;;     -
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Shackle for IDEs. [TODO]
;; 
;; what I want:
;;
;; +-------+--------+
;; |       | errors |
;; | file  |        |
;; |       +--------+
;; |       | shell  |
;; +-------+--------+
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;(require 'shackle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defconst ... "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-shackle-rule ()
  "
  "
  (interactive)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-shackle-add-rule! (ShackleRule)
  "Register the `ShackleRule' (onto `shackle-rules').
  "
  (add-to-list 'shackle-rules ShackleRule)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONFIGURATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package shackle

  :config
  (progn
    
    (setq shackle-rules
          '(("*Flycheck errors*" :regexp t :align 'below :size 10)
            ("*compilation*"     :regexp t :align 'below :size 0.4)))

    (shackle-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. 
;;
;;   ("..." :regexp t :popup t :select t :align bottom)
;;

;; `shackle-rules`:
;; 
;; a list of rules. 
;; 
;; Each rule consists of:
;; 
;; - a condition, and
;; - a set of key-value-pairs
;; 
;; the key-value-pairs tell what to do with the buffer in question.
;; 
;; The condition can be either: 
;; 
;; - a symbol, 
;; - a string,
;; - or a list of either.
;; 
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-shackle)