;;==============================================;;

;;;###autoload
(defun describe-function (function)
  "Display the full documentation of FUNCTION (a symbol).
When called from lisp, FUNCTION may also be a function object."
  (interactive
   (let* ((fn (function-called-at-point))
          (enable-recursive-minibuffers t)
          (val (completing-read
                (if fn
                    (format "Describe function (default %s): " fn)
                  "Describe function: ")
                #'help--symbol-completion-table
                (lambda (f) (or (fboundp f) (get f 'function-documentation)))
                t nil nil
                (and fn (symbol-name fn)))))
     (unless (equal val "")
       (setq fn (intern val)))
     (unless (and fn (symbolp fn))
       (user-error "You didn't specify a function symbol"))
     (unless (or (fboundp fn) (get fn 'function-documentation))
       (user-error "Symbol's function definition is void: %s" fn))
     (list fn)))

  ;; We save describe-function-orig-buffer on the help xref stack, so
  ;; it is restored by the back/forward buttons.  'help-buffer'
  ;; expects (current-buffer) to be a help buffer when processing
  ;; those buttons, so we can't change the current buffer before
  ;; calling that.
  (let ((describe-function-orig-buffer
         (or describe-function-orig-buffer
             (current-buffer))))

    (help-setup-xref
     (list (lambda (function buffer)
             (let ((describe-function-orig-buffer
                    (if (buffer-live-p buffer) buffer)))
               (describe-function function)))
           function describe-function-orig-buffer)
     (called-interactively-p 'interactive))

    (save-excursion
      (with-help-window (help-buffer)
        (if (get function 'reader-construct)
            (princ function)
          (prin1 function))
        ;; Use " is " instead of a colon so that
        ;; it is easier to get out the function name using forward-sexp.
        (princ " is ")
        (describe-function-1 function)
        (with-current-buffer standard-output
          ;; Return the text we displayed.
          (buffer-string))))
    ))

;;==============================================;;

;; `with-help-window':
;;
;; Evaluate BODY, send output to BUFFER-OR-NAME and show in a help window.
;; This construct is like ‘with-temp-buffer-window’ but unlike that
;; puts the buffer specified by BUFFER-OR-NAME in ‘help-mode’ and
;; displays a message about how to delete the help window when it’s no
;; longer needed.  The help window will be selected if
;; ‘help-window-select’ is non-nil.
;; Most of this  is done by ‘help-window-setup’, which see.

;;==============================================;;
