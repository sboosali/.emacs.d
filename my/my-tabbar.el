(provide 'my-tabbar)
(require 'tabbar)
(require 'etc)
(require 's)
(require 'dash)

(defun haskell/tabbar-buffer-groups ()
  "overrides tabbar-buffer-groups defined in my-tabbar.
 puts \".note\" files into User not Notes" 
  (list
   (cond
    ((s-matches? "\.hs" (buffer-name))  ;;(my/ends-with (buffer-name) ".hs") ;; e.g. "Etc.hs<Commands>"
     "Haskell")
    ((eq major-mode 'dired-mode)
     "Dired")
    ((-contains? (list "*Quail Completions*" "*Messages*" "*haskell-process-log*" "*scratch*") (buffer-name))
     "etc")
    (t
     "User")
    )))

(setq tabbar-buffer-groups-function 'haskell/tabbar-buffer-groups)

(global-set-key [M-s-left] 'tabbar-backward)
(global-set-key [M-s-right] 'tabbar-forward)

;last
(tabbar-mode t)
