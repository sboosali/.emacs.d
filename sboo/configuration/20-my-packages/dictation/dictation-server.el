;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'epc)
(require 'epcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dictation-server-port
  3250
  "")

(defvar dictation-server-process
  nil
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictation-server-initialize (manager)
  (epc:define-method manager
                     'echo
                     )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictation-server-start ()
  (epcs:server-start #'dictation-server-initialize
                     dictation-server-port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictation-server-set-process ()
  (progn
    (setq dictation-server-process
          (dictation-server-start))
    dictation-server-process))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
(dictation-server-set-process)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes

;; See
;;    - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dictation-server)