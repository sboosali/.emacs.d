;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOOKMARKS
;;
;; > Emacs's Bookmarks is similar to browser's bookmark. 
;; > It lets you easily open frequently needed files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (setq bookmark-save-flag 1)

  ;; ^ everytime the bookmark-file is changed, automatically save it.

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M-x list-bookmark
;;

;; M-x bookmark-set
;;
;; 【Ctrl+x r m】
;;
;; adds the current file (or directory) to the bookmark-file.
;; prompts you for a name.
;;

;; See
;;     - http://ergoemacs.org/emacs/bookmark.html
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-settings-bookmarks)