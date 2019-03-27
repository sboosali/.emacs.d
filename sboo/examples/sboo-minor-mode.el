(make-variable-buffer-local
   (defvar sboo-count 0
     "Number of sboos inserted into the current buffer."))

(defun insert-sboo ()
  ""
  (interactive)
  (setq sboo-count (1+ sboo-count))
  (insert "sboo"))

(define-minor-mode sboo-mode

  "Get your sboos in the right places."

  :lighter " sboo"

  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'insert-sboo)
            map)

  ())

