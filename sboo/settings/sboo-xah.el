;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (some of) XahLee's utilites
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xah-new-empty-buffer () 

  "Create a new empty buffer.
  New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.
  
  It returns the buffer (for elisp programing).
  
  URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
  Version 2017-11-01" 
  
  (interactive) 
  
  (let (($buffer (generate-new-buffer "untitled")))
  
    (progn
      (switch-to-buffer $buffer)
      (funcall initial-major-mode)
      (setq buffer-offer-save t) 
      $buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq initial-major-mode 'text-mode)
;; ^ Set Default Major Mode

(setq initial-buffer-choice 'xah-new-empty-buffer)
;; ^ Start Emacs with Empty Buffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; List Major Modes:
;;
;; You can view a list of major mode names by Alt+x `describe-variable' on `auto-mode-alist':
;; 
;;     (describe-variable 'auto-mode-alist)
;;

;; list-matching-lines
;;
;; ^ M-x `list-matching-lines' shows all lines in a buffer that match some regexp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-xah)