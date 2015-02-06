(provide 'notes-app)

(defun notes-app ()
  ;(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
  (find-file "~/Dropbox/any.note")
  (end-of-buffer)
  (split-window-vertically)
)

(if (string-match "Notes\\.app" (getenv "EMACSPATH"))
    (notes-app))

