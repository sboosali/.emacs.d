#!/usr/bin/emacs --script

(defvar listen-port 55880 ;;TODO
    "port of the server")

(defvar listen-host "127.0.0.1"
    "host of the server")

(defun listen-start nil
    "starts an emacs tcp client listener"
    (interactive)
    (make-network-process
     :name     "listen"
     :buffer   "*listen*"
     :family   'ipv4
     :host     listen-host
     :service  listen-port
     :sentinel 'listen-sentinel
     :filter   'listen-filter))

(defun listen-stop nil
  "stop an emacs tcp listener"
  (interactive)
  (delete-process "listen"))

(defun listen-filter (proc string)   
  (message string))

(defun listen-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (message (format "client %s has quit" proc))))

(defun tcp-client-main ()
  (listen-start)
  (sleep-for 5)
  (listen-stop))

(provide 'tcp-client)