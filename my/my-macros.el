(provide 'my-macros)

;;;;;;;;;;;;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;
; from
;; kmacro-start-macro
;; kmacro-name-last-macro
;; insert-kbd-macro

(fset 'munge-facebook-songs
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([down 5 M-left M-left left 11 18 98 121 67108896 1 134217848 down 11 11 down] 0 "%d")) arg)))

