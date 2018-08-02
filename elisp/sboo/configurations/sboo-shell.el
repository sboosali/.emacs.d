;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SHELL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; the `shell` builtin-package.
;;
;; `shell-mode` *not* a terminal-emulator. 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package shell
  :bind
  ( ("C-c s" . shell)
    :map term-mode-map
    ("<kp-prior>" . term-send-up)
    ("<kp-next>"  . term-send-down)
    ("<tab>"      . self-insert-command))) ;;TODO


(progn

  ;; the `shell-mode` major-mode has 
  ;; the `shell-mode-map` keymap.

  ;;;;(define-key comint-mode-map (kbd "<kp-prior>") 'comint-previous-input)
  (define-key  shell-mode-map (kbd "<kp-prior>") 'comint-previous-input)
  ;; <prior> is the page-up key

  ;;;;(define-key comint-mode-map  (kbd "<kp-next>") 'comint-next-input)
  (define-key  shell-mode-map  (kbd "<kp-next>") 'comint-next-input)
  ;; <next> is the page-down key

)

;; TODO `comint-mode-map`???
;; ^ many shell modes inherit from `comint-mode`,
;; like the (simple, built-in) `shell-mode`.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; relevant `shell-*` functions and variables:
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-shell)