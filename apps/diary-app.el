(provide 'diary-app)


(defun diary-app ()
  (find-file "~/diary/dreams")
  (end-of-buffer)
  (find-file "~/TODO")
  (end-of-buffer)

  ;(split-window-vertically)

 ; (require 'my-mail) ; lazily load
 ; (mu4e); will not build 

  (find-file "~/diary/work.org")
  (find-file "~/diary/home.org")
  (find-file "~/diary/chores.org")
  (end-of-buffer)

  (org-todo-list nil)
  (other-window 1)


)

(when-app "Diary\\.app" 'diary-app)
