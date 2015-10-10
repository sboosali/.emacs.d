(provide 'commands-app)
(require 'my-compilation)
(require 'my-haskell)

(require 's)


(defvar commands-directory "~/commands-spiros")
(defun commands-file (filename)
  (concat commands-directory "/" filename))

(defun commands-at-work ()
  (setq tabbar-buffer-groups 'haskell/tabbar-buffer-groups)                               

  (find-file (commands-file "notes"))
  (end-of-buffer)

  (shell "*shell*")
  (insert "cabal build")  (comint-send-input)
  (compilation-shell-minor-mode)
  (insert "make check")  (comint-send-input)
  (insert "find config")  (comint-send-input)
  (end-of-buffer)

  ;; (shell "*shell2*")
  ;; (compilation-shell-minor-mode)
  ;; (insert "make serve")  (comint-send-input)
  ;; (end-of-buffer)

  (find-file "~/.emacs.d/apps/commands-app.el") ; this file

  (find-file (commands-file "commands-spiros.cabal"))
  (end-of-buffer)
  ;; (split-window-vertically)

; opening files must come after opening the cabal file which loads Haskell mode 
  (find-file (commands-file "config/Commands/Plugins/Spiros/*.hs") t)
  (find-file (commands-file "config/Commands/Plugins/Spiros/*/*.hs") t)
  (find-file (commands-file "config/Commands/Plugins/Spiros/*/*/*.hs") t)
  (find-file (commands-file "config/Commands/Plugins/Spiros/Main.hs")) ; loads Haskell-mode
  (run-with-idle-timer 5 nil (lambda () (shell))) ; go back to buff, after Haskell-mode loads

;  (tabbar-mode) ; tab on each window (not one per frame)
  )

(defun commands-app ()
  ;; (when-host "odysseus" 'commands-at-home)
  (when-host "c02m71hdfd58" 'commands-at-work)
  )

(when-app "Commands\\.app" 'commands-app)
