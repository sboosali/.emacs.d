(provide 'my-erc)
(require 'my-secret) 

(require 'erc)

;;; http://www.gnu.org/software/emacs/manual/html_mono/erc.html#Sample-Session

;;; Connect to Freenode
; Run M-x erc. Use “irc.freenode.net” as the IRC server, “” as the port, and choose a nickname.
(erc
 :server "irc.freenode.net"
 :port 6667
 :nick secret-erc-nickname
 :password secret-erc-password
 )

;; /msg NickServ identify <password>

;;; join channel
; /join #haskell

;; Register your nickname with Freenode
;; /msg NickServ register <password>
