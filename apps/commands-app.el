(provide 'commands-app)
(require 'my-compilation)
(require 'my-haskell)

(require 's)

(defvar commands-directory "~/commands-spiros")
(defun commands-file (filename)
  (concat commands-directory "/" filename))

(defun commands-at-work ()
  (setq tabbar-buffer-groups 'haskell/tabbar-buffer-groups)                               
  (key (kbd "M-u") 'compile)

  (find-file (commands-file "notes"))
  (end-of-buffer)

  (shell)
  (insert "cabal build")  (comint-send-input)
  (compilation-shell-minor-mode)
  (insert "make check")  (comint-send-input)
  (insert "find config")  (comint-send-input)
  (end-of-buffer)

  (shell "*shell-commands*")
  (compilation-shell-minor-mode)
  (insert "cabal run")  (comint-send-input)

  (find-file "~/.emacs.d/apps/commands-app.el") ; this file

  (find-file (commands-file "commands-spiros.cabal"))
  (end-of-buffer)
  (split-window-vertically)

; opening files must come after opening the cabal file which loads Haskell mode 
;  (find-file (commands-file "config/Commands/Plugins/Spiros/*.hs"))
  (find-file (commands-file "config/Commands/Plugins/Spiros/Root.hs"))
  (find-file (commands-file "config/Commands/Plugins/Spiros/Phrase.hs"))
  (find-file (commands-file "config/Commands/Plugins/Spiros/Shortcut.hs"))
  (find-file (commands-file "config/Commands/Plugins/Spiros/Shim.hs"))
  (find-file (commands-file "config/Commands/Plugins/Spiros.hs")) ; loads Haskell-mode
  (run-with-idle-timer 5 nil (lambda () (shell))) ; go back to buff, after Haskell-mode loads

;  (tabbar-mode) ; tab on each window (not one per frame)
  )

(defun commands-app ()
  ;; (when-host "odysseus" 'commands-at-home)
  (when-host "c02m71hdfd58" 'commands-at-work)
  )

(when-app "Commands\\.app" 'commands-app)
