
(deftheme sboo-inverse


  "My default theme

Related: `sboo-default-face'

Links: URL `http://emacsfodder.github.io/blog/notes-on-deftheme/'.")

;;----------------------------------------------;;
;; Faces ---------------------------------------;;
;;----------------------------------------------;;

(defvar sboo-theme-palette

   '(
      (color-1 "#ffffff")
      (color-2 "#ff0000")
      (color-3 "#00ff00")
      (color-4 "#0000ff")
   )

"")

  ;; Set faces:

(custom-theme-set-faces 'sboo-inverse

   '(default ((t (:inherit sboo-default-face))))

 '('sboo-inverted-face ((t (:inverse-video t :inherit '(sboo-default-face)))))

)

   ;; `(default ((t (:foreground ,color-1 :background black))))

   ;; `(cursor  ((t (:background ,color-4))))

  ;; Set variables:

(custom-theme-set-variables 'sboo-inverse

   '(any-variable EXPR)
)

(provide-theme 'sboo-inverse)
