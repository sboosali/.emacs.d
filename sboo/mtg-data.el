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

(require 'cl)
(require 'pcase)
(require 'seq)

(require 'json)

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;

(defun mtg--is-list-of-string (value)

  "`listp' of `stringp'"

  (and (listp value)
       (seq-every-p #'stringp value)))

;;----------------------------------------------;;

(cl-defun mtg--deserialize (&key file symbol predicate)

  "Read FILE into SYMBOL, validating with PREDICATE."

  (interactive (list
                (read-file-name "Read from file: ")
                (read-string "Deserialize into variable: ")
                (intern-soft (completing-read "Validate type (optional): " obarray nil #'functionp))
                ))

  (with-temp-buffer

    (insert-file-contents file)
    (goto-char (point-min))

    (let* ((VALUE  (read (current-buffer)))
           (VALID? (if (fboundp predicate)
                       (funcall predicate VALUE)
                     t))
           )

      (if VALID?
          (set symbol VALUE)))))

;;----------------------------------------------;;

(cl-defun mtg-data--load-card-names (&key file)

  "Load `mtg-card-names' from FILE.

Inputs:

• FILE — a « .json » or « .el » file.
  by default, is `mtg-card-names-file-default'.

Output:

• a .

Example:

• M-: (mtg-data--load-card-names :file \"scryfall-default-cards.json\")
    ⇒ 

Links:

• URL Related:'"

  (let* ((FILE           (or (bound-and-true-p file)
                             mtg-card-names-file-default))
         (FILE-EXTENSION (file-name-extension FILE))

         (FILE-NAME (pcase FILE-EXTENSION
                      ('nil (concat FILE ".list.el"))                  ;TODO; (load NAME) when empty file-extension.
                      (_    FILE)))
         )

    (mtg--deserialize :file      FILE-NAME
                      :symbol    'mtg-card-names
                      :predicate #'mtg--is-list-of-string)))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(cl-defun mtg-card-names (&key force)

  "Accessor for variable `mtg-card-names'.

Inputs:

• FORCE — a boolean.

Initialize `mtg-card-names' from `mtg-card-names-file-default', 
if necessary (or if FORCE is set)."

  (if (or force
          (not mtg-card-names))
      (mtg-data--load-card-names))

  mtg-card-names)

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defcustom mtg-card-names-file-default

  "mtg-card-names"

  "Default filename (or basename) for `mtg-card-names'.

File extension may be:

• ∅
• « .json »
• « .txt »
• « .el »
• « .list.el »"

  :type  '(string :tag "Filename / Basename")

  :safe  t

  :group 'mtg)

;;----------------------------------------------;;

(defcustom mtg-card-names

  nil

  "List of all MTG card names."

  :type  '(list string)

  :safe  t

  :group 'mtg)

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;; 
;; 
;; 
;;----------------------------------------------;;
(provide 'mtg-data)