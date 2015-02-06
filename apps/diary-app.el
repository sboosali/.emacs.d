(provide 'diary-app)

(defun diary-app ()
 (find-file "~/diary/sleep")
 (find-file "~/TODO")
)

(if (string-match "Diary\\.app" (getenv "EMACSPATH"))
    (diary-app))

