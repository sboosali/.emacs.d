(provide 'my-keymaps) 

;; key maps 

(defvar my-keymap (make-sparse-keymap))
;; (defvar my-keymap-prefix (kbd "M-q"))
(defvar my-keymap-prefix (kbd "C-x C-y"))

(global-unset-key my-keymap-prefix)
(global-set-key   my-keymap-prefix my-keymap)

(define-key my-keymap "c" 'compile) 

