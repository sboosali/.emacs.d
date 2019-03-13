;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Data for `mtg'.
;;
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(require 'json)

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(cl-defun mtg-data--load-card-names (&key file)

  "Load `mtg-card-names' from FILE.

Inputs:

• FILE — a « .json » or « .el » file.
  by default, is `mtg-card-names-file-default'.

Output:

• a .

Example:

• M-: (sboo-mtg-data--load-card-names )
    ⇒ 

Links:

• URL Related:'"

  (let* (()
         )

    ))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defcustom sboo-mtg-card-names-file-default

  "mtg-card-names.txt"

  "Default filename for `mtg-card-names'.

File extension may be:

• « .json »
• « .txt »
• « .el »
• « .list.el »"

  :type  '(string :tag "Filename")

  :safe  t

  :group 'sboo)


;;----------------------------------------------;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;; 
;; 
;; 
;;----------------------------------------------;;
(provide 'mtg-data)