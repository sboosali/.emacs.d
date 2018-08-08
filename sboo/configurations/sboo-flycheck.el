;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck


  :defer t
  ;;^ deferred because flycheck is "more framework than application". i.e. any "application" package will `require` it whenever needed (e.g. `dante`), and afaik, it's not useful alone. 

)

;; See
;;     - https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-flycheck.el
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-flycheck)