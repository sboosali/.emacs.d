(provide 'observations-app)
;(require 'edit-server)
;(require 'my-edit-server)



(defun obs-app ()

;  (edit-server-start)

  (require 'my-erc)

;  (find-file "~/Dropbox/.obs")
  (find-file "~/config/.profile")
;  (find-file "~/Haskell")
;  (find-file "~/chrome_extensions/commands-context-chrome/manifest.json")
  (find-file "~/.emacs.d/init.el")
  ; last sets $PWD

  (shell)
  (insert (concat "find " HOME "apps"))  (comint-send-input)
  (insert (concat "find " HOME "my"))  (comint-send-input)
  (insert "find ~/config | grep -v '.git' | grep -v 'Spiros.ddictateprofile4'")  (comint-send-input)
  (split-window-vertically)
  (other-window 0)

  ;; (insert "cd ~/voice/commands-backends-osx9")  (comint-send-input)
  ;; (insert "find sources")  (comint-send-input)
  ;; (find-file "commands-backends-osx9.cabal")
  ;; (find-file "~/voice/commands-backends-osx9/sources/*/*/*/*.hs" t)


  ;; (find-file "~/things")
  ;; (end-of-buffer)

  (find-file "~/.emacs.d/apps/commands-app.el")
)

(when-app "Obs\\.app" 'obs-app)
