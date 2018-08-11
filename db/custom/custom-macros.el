;; -*- buffer-read-only: t -*-

;;TODO buffer read-only
;;TODO `sboo-kmacro-save-last-macro'

;; `buffer-read-only' directive:
;;
;; > Only use `mode:' to set the major mode, and use `eval:' to enable minor modes.
;;
;; Either of the following directives should make the file be automatically read as read-only:
;;
;;     ;; -*- eval: (read-only-mode 1) -*-
;;     ;; -*- buffer-read-only: t -*-
;;
;; See 
;;     - https://emacs.stackexchange.com/questions/3994/make-buffer-read-only-on-a-per-file-basis
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserted by `kmacro-name-last-macro'...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'sboo-macro-shiftableKeyDescription
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([11 115 104 105 102 116 97 98 108 101 75 101 tab right delete delete delete delete delete 134217828 backspace 32 48 48 22 left backspace 134217826 39 39 32 39 39 32 end right 11 right left 134217828 delete delete delete delete 134217828 134217826 134217826 134217830 right right 22 backspace end right 11 end backspace left 11 home left 134217826 left left 22 end 32 11 11 11 11 11 11 134217828 delete delete 40 67 111 100 101 115 end 134217828 return backspace delete end 41 right 134217828 134217828 delete delete delete right left 11 home up 19 99 111 100 101 right 134217826 left 40 41 32 left left 22 end right 11 11 11 11 11 11 11 11 11 end down] 0 "%d")) arg)))

