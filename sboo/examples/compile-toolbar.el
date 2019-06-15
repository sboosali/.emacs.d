;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Configure the `tool-bar' package.
;;
;; • 
;; • 
;;
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(require 'cl-lib)
(require 'pcase)

(require 'tool-bar)

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-tool-bar-map

  (when (keymapp tool-bar-map)

    (let ((MAP (copy-keymap tool-bar-map))
          )

      (define-key MAP [undo]        nil)
      (define-key MAP [separator-2] nil)

      (define-key-after MAP [separator-sboo] menu-bar-separator)

      (tool-bar-local-item
       "left-arrow" #'previous-error-no-select 'previous-error-no-select MAP
       :rtl "right-arrow"
       :help "Goto previous error")

      (tool-bar-local-item
       "right-arrow" #'next-error-no-select 'next-error-no-select MAP
       :rtl "left-arrow"
       :help "Goto next error")

      (tool-bar-local-item
       "cancel" #'kill-compilation 'kill-compilation MAP
       :enable '(let ((buffer (compilation-find-buffer)))
		  (get-buffer-process buffer))
       :help "Stop compilation")

      (tool-bar-local-item
       "refresh" #'recompile 'recompile MAP
       :help "Restart compilation")

      MAP))

  "my Tool Bar.

Add some Icons/Commands to the standard Tool Bar.")

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Configuration -------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; `tool-bar-local-item':
;; 
;;     (tool-bar-local-item ICON-STRING KEY-DEFINITION KEY-SYMBOL TOOLBAR-KEYMAP &key help enable rtl)
;;
;; > ICON is the base name of a file containing the image to use.  The
;; > function will first try to use low-color/ICON.xpm if ‘display-color-cells’
;; > is less or equal to 256, then ICON.xpm, then ICON.pbm, and finally
;; > ICON.xbm, using ‘find-image’.
;;
;; > KEY-DEFINITION is the key definition.
;; > KEY-SYMBOL is a symbol for the fake function key in the menu keymap.  
;;

;; 
;;----------------------------------------------;;
(provide 'sboo-tool-bar)