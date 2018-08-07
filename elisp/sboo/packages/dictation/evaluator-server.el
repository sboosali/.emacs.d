;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'epc)
(require 'epcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evaluator-server-port
  3250
  "")

(defvar evaluator-server-process
  nil
  "")

(defvar evaluator-server-method--eval
  'eval-emacs-function
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluator-server-initialize (manager)
  (epc:define-method manager evaluator-server-method--eval-emacs-function
                     (lambda (fname args)
                       (apply (intern fname) args))))

(defun evaluator-server-stop ()
  (epcs:server-stop evaluator-server-process))

(defun evaluator-server-start ()
  (setq evaluator-server-process
        (epcs:server-start #'evaluator-server-initialize
                           evaluator-server-port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evaluator-server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes

;; See
;;    - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'evaluator-server)