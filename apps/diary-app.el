(provide 'diary-app)


(defun diary-app ()
 (find-file "~/diary/sleep")
  (end-of-buffer)
 (find-file "~/TODO")
  (end-of-buffer)

  ;(split-window-vertically)

 ; (require 'my-mail) ; lazily load
 ; (mu4e); will not build 

  (find-file "~/Dropbox/example.org")
  (end-of-buffer)

)

(if (string-match "Diary\\.app" (getenv "EMACSPATH"))
    (diary-app))

