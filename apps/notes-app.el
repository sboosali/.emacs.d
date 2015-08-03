(provide 'notes-app)
(require 'centered-cursor-mode)
(require 'smooth-scrolling)


(defun notes-app ()
  ;(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
  (find-file "~/Dropbox/any.note")
  ;(split-window-vertically)

  ;

  ;(global-centered-cursor-mode)

)

(when-app "Notes\\.app" 'notes-app)

