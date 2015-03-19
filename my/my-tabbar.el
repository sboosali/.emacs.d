(provide 'my-tabbar)
(require 'tabbar)
(require 'etc)


(tabbar-mode t)

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function group all buffers into a number of groups equal to the number
 of cases, each named by the results of its case."
  (list
   (cond
    ((my/ends-with (buffer-name) ".hs")
     "Haskell")
    ((my/ends-with (buffer-name) ".note")
     "Notes")
    ((eq major-mode 'dired-mode)
     "Dired")
    (t
     "User")
    )))




(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

(global-set-key [M-s-left] 'tabbar-backward)
(global-set-key [M-s-right] 'tabbar-forward)

