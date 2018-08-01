;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emacs Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun server-start-unless-running ()
  "Run `server-start`,
   unless another Emacs Server is already running (for the same user).
   Platform-compability: only Unix."
  (interactive)
  (let* ((tmp
          "/tmp")
          ;; (getenv "TMPDIR"))
          ;; ;; ^ i.e. "$TMPDIR"
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
  ;; ^ via @markhellewell

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun server-start-once ()
;;   "Start an Emacs Server for the `emacsclient` command, 
;;   unless a server is already running.
;;   We check for the presence of another Emacs Server 
;;   via the existence of a specific socket; for example,
;;   named \"/tmp/emacs1001/server\".
;;   `server-start-once` is idempotent, modulo race conditions."
;;   (interactive)
;;   (let 
;;       ( (server-socket-file "/tmp/emacs1001/server") ;;TODO shell out for "$UID"
;;          ;; ^
;;          ;; e.g. "/tmp/emacs1001/server"
;;          ;; i.e. "/tmp/emacs<UserId>/<ServerName>"
;;          ;; 
;;          ;; NOTE when evaluated from a running Emacs Server,
;;          ;; `server-socket-file` should equal `(concat server-socket-dir server-name)`
;;          ;; (otherwise, some `server-*` variables are undefined).
;;       )
;;     (unless (file-exists-p server-socket-file)
;;       ;; ^ check whether another server (named `server-name`) is already running.
;;       (server-start)))
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-server)