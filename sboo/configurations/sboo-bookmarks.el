;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bookmarks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  
  :init
  (setq
   
   bookmark-default-file (sboo-database-file "bookmarks" "bookmarks.el")
   ;; ^ the file in which to save bookmarks, by default.

   bookmark-save-flag 1))
   ;; ^ `1` means: "each command that sets a bookmark will also save your bookmarks".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Bookmarks are somewhat like registers in that they record positions you can jump to. Unlike registers, they have long names, and they persist automatically from one Emacs session to the next. 
;; The prototypical use of bookmarks is to record where you were reading in various files.

;; ;; See:
;; 
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Bookmarks.html
;;
;; -
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-bookmarks)