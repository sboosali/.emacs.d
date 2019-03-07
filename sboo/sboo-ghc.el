;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for the `ghc' package.
;;
;; See:
;;
;; * `sboo-excluded-directories'.
;; * `sboo-excluded-file-names'
;; * `sboo-excluded-file-extensions'.
;;
;; 
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(require 'cl)
(require 'pcase)

;;

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;

(defun sboo-ghc-get-name-of-prior-definition ()
 
  "Return the name of the previous haskell definition (from `point').

Output:

• a string.

Calls `haskell-ds-move-to-decl'."

  (save-excursion

    (let ((is-direction-forwards?  nil)
          (is-haskell-literate?    nil)
          (is-movement-idempotent? t)
          )
      (haskell-ds-move-to-decl is-direction-forwards? is-haskell-literate? is-movement-idempotent?))

    (thing-at-point 'symbol :no-properties)))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-ghc-pragma-read ()

  "Read a GHC pragma, plus any of that pragma's required inputs.

Output:

• a list of strings.

Related:

• `sboo-ghc-pragma-read-pragma'.
• `sboo-ghc-pragmas-alist'."

  (interactive)

  (completing-read (format "%s: " prompt)
                   candidates))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-pragma ()

  "Read a GHC pragma.

Related:

• `sboo-ghc-pragmas-alist'."

  (interactive)

  (let ((prompt     "Pragma")
        (candidates sboo-ghc-pragmas)
        )

    (completing-read (format "%s: " prompt)
                     candidates)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read- ()

  "Read a .

Inputs:

• 

Output:

• 

Related:

• `sboo-ghc-pragmas-alist'."

  (interactive)

  (let ((prompt     "")
        (candidates )
        )

  (sboo-ghc-pragma-read prompt candidates)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-string ()

  "Read a haskell string.

Escapes:

• double-quotes (i.e. « \" »).

Related:

• `sboo-ghc-pragmas-alist'."

  (interactive)

  (let ((prompt     "String")
        (candidates nil)
        )

  (sboo-ghc-pragma-read prompt candidates)))

;;----------------------------------------------;;

(defun sboo-ghc-read-haskell-variable ()

  "Read a haskell variable.

Inputs:

• 

Output:

• 

Related:

• `sboo-ghc-pragmas-alist'."

  (interactive)

  (let ((prompt     "")
        (candidates )
        )

  (sboo-ghc-pragma-read prompt candidates)))

;;----------------------------------------------;;

;;----------------------------------------------;;

(defun sboo-ghc-pragma-insert-pragma (&optional pragma)

  "Insert a GHC pragma, reading it if PRAGMA is nil.

Related:

• `sboo-ghc-pragma-read-pragma'."

  (interactive (list
                (sboo-ghc-pragma-read-pragma)))

  (let* ((STRING (format "{-# %s #-}" pragma))
         )

    (insert STRING)))

;;----------------------------------------------;;

;;----------------------------------------------;;
;; Aliases -------------------------------------;;
;;----------------------------------------------;;

(defalias 'sboo-ghc-read-WARNING    #'sboo-ghc-read-string)
(defalias 'sboo-ghc-read-DEPRECATED #'sboo-ghc-read-string)

     ;; ("LANGUAGE"     . sboo-ghc-read-LANGUAGE)

     ;; ("OPTIONS_GHC"  . sboo-ghc-read-OPTIONS_GHC)

     ;; ("MINIMAL"      . sboo-ghc-read-MINIMAL)
     ;; ("COMPLETE"     . sboo-ghc-read-COMPLETE)

     ;; ("INLINE"       . sboo-ghc-read-INLINE)
     ;; ("NOINLINE"     . sboo-ghc-read-INLINE)
     ;; ("INLINABLE"    . sboo-ghc-read-INLINABLE)
     ;; ("CONLIKE"      . sboo-ghc-read-CONLIKE)

     ;; ("RULES"        . sboo-ghc-read-RULES)

     ;; ("SPECIALIZE"   . sboo-ghc-read-SPECIALIZE)

     ;; ("OVERLAPPING"  . sboo-ghc-read-OVERLAPPING)
     ;; ("OVERLAPPABLE" . sboo-ghc-read-OVERLAPPABLE)
     ;; ("OVERLAPS"     . sboo-ghc-read-OVERLAPS)
     ;; ("INCOHERENT"   . sboo-ghc-read-INCOHERENT)

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;

(defcustom sboo-ghc-pragmas-alist ;TODO; make internal variable « hashtable ».

  '( ("LANGUAGE"     . sboo-ghc-read-LANGUAGE)

     ("OPTIONS_GHC"  . sboo-ghc-read-OPTIONS_GHC)

  ;; ("INCLUDE"      . sboo-ghc-read-INCLUDE)

     ("WARNING"      . sboo-ghc-read-WARNING)
     ("DEPRECATED"   . sboo-ghc-read-DEPRECATED)

     ("MINIMAL"      . sboo-ghc-read-MINIMAL)
     ("COMPLETE"     . sboo-ghc-read-COMPLETE)

     ("INLINE"       . sboo-ghc-read-INLINE)
     ("NOINLINE"     . sboo-ghc-read-INLINE)
     ("INLINABLE"    . sboo-ghc-read-INLINABLE)
     ("CONLIKE"      . sboo-ghc-read-CONLIKE)

     ("RULES"        . sboo-ghc-read-RULES)

     ("SPECIALIZE"   . sboo-ghc-read-SPECIALIZE)

     ("UNPACK"       . nil)
     ("NOUNPACK"     . nil)

     ("OVERLAPPING"  . sboo-ghc-read-OVERLAPPING)
     ("OVERLAPPABLE" . sboo-ghc-read-OVERLAPPABLE)
     ("OVERLAPS"     . sboo-ghc-read-OVERLAPS)
     ("INCOHERENT"   . sboo-ghc-read-INCOHERENT)

     ;; ("LINE"         . sboo-ghc-read-LINE)
     ;; ("COLUMN"       . sboo-ghc-read-COLUMN)
     ;; ("SOURCE"       . sboo-ghc-read-SOURCE)
     )

  "GHC Pragmas and elisp commands (to read them from the user)."

  :type '(alist :key-type   (string   :tag "Pragma")
                :value-type '(choice (const nil)
                                     (function :tag "Completer/Reader"))
                )

  :safe t

  :group 'sboo)


  ;; (dolist (PRAGMA-CONS sboo-ghc-pragmas-alist)
  ;;   (pcase PRAGMA-CONS
  ;;     (`(,PRAGMA-STRING . ,READ-PRAGMA)
  ;;      )))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; =======================
;; `ghc' Pragmas
;; =======================

;; 1. LANGUAGE pragma
;; 2. OPTIONS_GHC pragma
;; 3. INCLUDE pragma
;; 4. WARNING and DEPRECATED pragmas
;; 5. MINIMAL pragma
;; 6. INLINE and NOINLINE pragmas
;; 6.1. INLINE pragma
;; 6.2. INLINABLE pragma
;; 6.3. NOINLINE pragma
;; 6.4. CONLIKE modifier
;; 6.5. Phase control
;; 7. LINE pragma
;; 8. COLUMN pragma
;; 9. RULES pragma
;; 10. SPECIALIZE pragma
;; 10.1. SPECIALIZE INLINE
;; 10.2. SPECIALIZE for imported functions
;; 10.3. Obsolete SPECIALIZE syntax
;; 11. SPECIALIZE instance pragma
;; 12. UNPACK pragma
;; 13. NOUNPACK pragma
;; 14. SOURCE pragma
;; 15. COMPLETE pragmas
;; 16. Disambiguating between multiple COMPLETE pragmas
;; 17. OVERLAPPING, OVERLAPPABLE, OVERLAPS, and INCOHERENT pragmas

;; See:
;;
;; - <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#pragmas>
;;

;;----------------------------------------------;;
(provide 'sboo-ghc)