;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for an Emacs Server.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun server-start-unless-running ()

  "Run `server-start` unless another Emacs Server is already running.
  
  Platform-Compability: UNIX only.
  
  By @markhellewell
  "
  (interactive)

  (let* ((tmp
          "/tmp")
          
          ;; ^ i.e. "$TMPDIR"

         (uid
          (number-to-string (user-real-uid)))
          
          ;; ^ i.e. "$UID"

         (server-socket-file
          (concat tmp "/" "emacs" uid "/" "server")))
          
          ;; ^
          ;; e.g. "/tmp/emacs1001/server"
          ;; i.e. "/tmp/emacs<UserId>/<ServerName>"

  (unless (file-exists-p server-socket-file)

    (server-start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; > server-running-p predicate will evaluate to t if the Emacs server is running, 
;; > irrespective of which Emacs session currently "owns" the server process.

;; See:
;;
;; - https://emacs.stackexchange.com/questions/31224/how-to-test-programmatically-whether-the-current-emacs-session-among-several

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-server)