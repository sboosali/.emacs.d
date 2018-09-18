;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :defer t
  ;;^
  ;; deferred because flycheck is "more framework than application".
  ;; i.e. any "application" package will `require` it whenever needed (e.g. `dante`), and afaik, it's not useful alone. 

  :bind ;;TODO move to hook for any `prog-mode'.
  (("<kp-begin>" . flycheck-list-errors)       ; i.e. KeyPad 5.
   ("<kp-right>" . flycheck-next-error)        ; i.e. KeyPad 6.
   ("<kp-left>"  . flycheck-previous-error)))  ; i.e. KeyPad 4.

;; See
;;     - https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-flycheck.el
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck-haskell
  :defer t

  :commands
  (flycheck-haskell--find-default-directory))

;; ^
;;
;; commands include:
;; 
;;  `flycheck-haskell-liquid-executable'
;;  `flycheck-haskell--find-default-directory'
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-flycheck)