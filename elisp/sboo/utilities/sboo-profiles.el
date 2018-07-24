;;TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; '''The way I use to maintain several .emacs.d directories in parallel is the following.

;; emacs is started like this:

;; alias emacs-windows='./result/bin/emacs -q --load "~/.emacs.d-windows/init.el"'
;; alias emacs-here='./result/bin


;; The patch which allows you to specify .emacs.d location via `EMACS_USER_DIRECTORY' environment variable is available but not merged.'''

;; e.g.
;; > M-: user-init-file

;; e.g. on Windows: 
;; > user-emacs-directory 
;; "c:/Users/Spiros/AppData/Roaming/.emacs.d/" 

(setq my-profile-name "emacs-sboo")
;;(setq my-profile-name "emacs-minimal")
;;(setq my-profile-name "emacs-default")
;; ^ distinguish this "profile" from others.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-profiles)