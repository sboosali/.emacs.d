(provide 'my-erc)
(require 'my-secret) 

(require 'erc)

(setq erc-modules '(
  autojoin
  button
  completion
  fill
  irccontrols
  list
  log
  match
  menu
  move-to-prompt
  netsplit
  networks
  noncommands
  notifications
  readonly
  ring
  stamp
  track))

(erc-update-modules)

;;; http://www.gnu.org/software/emacs/manual/html_mono/erc.html#Sample-Session

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(setq erc-autojoin-channels-alist
      '(("freenode.net"
	 ;; "#emacs"
	 "#haskell"
	 ;; "#xmonad"
	 ;; "#linux"
	 "#nixos"
	 )))
					; opened in order, last one is active
(add-hook 'erc-text-matched-hook 'erc-beep-on-match)
(setq erc-beep-match-types '(current-nick)) ; '(current-nick <keyword> ...)

;;; Connect to Freenode
; Run M-x erc. Use “irc.freenode.net” as the IRC server, “” as the port, and choose a nickname.
(defun my-erc() (interactive)
  (erc
 :server "irc.freenode.net"
 :port 6667
 :nick secret-erc-nickname
 :password secret-erc-password
 ))

;; /msg NickServ identify <password>

;;; join channel
; /join #haskell

;; Register your nickname with Freenode
;; /msg NickServ register <name> <email>
;; /msg NickServ register sboosali samboosalis@gmail.com
