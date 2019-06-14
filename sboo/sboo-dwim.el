;;; sboo-dwim.el --- Utilities for writing ‘*-dwim’ commands -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 13 Jun 2019
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities for writing “DWIM” (a.k.a. Do-What-I-Mean) commands.
;; 
;; Augment functions to “Do-What-I-Mean”.
;;
;; Replace a reader (e.g. `read-string'),
;; which always requires manual input,
;; with a “Do-What-I-Mean” completer (e.g. `sboo-dwim-string'),
;; which sometimes automatically guesses input.
;;
;; Guess user input via matching against (any of):
;;
;; • the `region'
;; • the clipboard
;; • `thing-at-point'
;;
;; Functions:
;;
;; • `sboo-dwim-get'
;;
;; Variables:
;;
;; • `sboo-dwim-*-default'
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile
  (require 'rx)
  (require 'pcase))

;;----------------------------------------------;;

(progn
  (require 'seq)
  (require 'cl-lib))

;;----------------------------------------------;;
;; Types ---------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-dwim nil

  "Customize DWIM utilities."

  :prefix "sboo-dwim/"
  :group 'lisp)

;;==============================================;;

(defcustom sboo-dwim-all-builtin-things

  '( symbol
     list
     sexp
     defun
     filename
     url
     email
     word
     sentence
     whitespace
     line
     page
    )

  "Builtin things for `forward-thing' (see `thingatpt').")

;; predicate #'sboo-dwim/textual-string-p

;; things    '(sentence line word)
           
;; prompt    "String: "

;; reader    #'read-string

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-dwim/textual-string-p (s)

  "Whether S “looks like” simple text.

Inputs:

• S — a `stringp'.

Output:

• a `booleanp'.
  Matches only Alphanumeric Characters and Whitespace Characters (see `rx')."

  (and (stringp s)

       (if (string-match-p (rx bos (1+ (or alphanumeric whitespace)) eos) s)
           t
         nil)))

;;----------------------------------------------;;

(defun sboo-dwim/numerical-string-p (s)

  "Whether S parses as a (nonzero) number.

Inputs:

• S — a `stringp'.

Output:

• a `booleanp'."

  (and (stringp s)

       (let ((n (string-to-number s)))
         (and (numberp n)
              (/= n 0)))))

;;----------------------------------------------;;

(defun sboo-dwim/interesting-character-p (char)

  "Whether CHAR is an “interesting” Unicode Character.

Inputs:

• CHAR — a `characterp' (which are a subset of `integerp's).

Output:

• a `booleanp'."

  (let* ((UNPRINTABLE?        (< char 32))  ;TODO what about unprintables in higher-ranges? or surrogates?
         (ASCII-ALPHANUMERIC? (or (and (>= char ?0) (<= char ?9))
                                  (and (>= char ?A) (<= char ?Z))
                                  (and (>= char ?a) (<= char ?z))
                                  ))
         )

    (and (not UNPRINTABLE?)
         (not ASCII-ALPHANUMERIC?)
         )))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-dwim-all-things (&key (fast nil))

  "Return all known “Things” (w.r.t. ‘thinatpt’).

Inputs:

• FAST — a `booleanp'.

Output:

• a `listp' of `symbolp's.

Example:

• M-: (sboo-dwim-all-things :fast nil)
    ⇒ '(symbol list sexp defun filename url email word sentence whitespace line page str page op line list word point button symbol sentence paragraph defun char comment whitespace thing sexp)

Related:

• `forward-thing'"

  (let* ((KNOWN-THINGS sboo-dwim-all-builtin-things)
         )

    (if fast
        KNOWN-THINGS

      (let* ((UNKNOWN-THINGS '())
             (ADD-THING       (lambda (symbol)
                                (when (sboo-thing-p symbol)
                                  (push symbol UNKNOWN-THINGS))))
             )

        (progn
          (mapatoms ADD-THING obarray)

          (let* ((THINGS (append KNOWN-THINGS UNKNOWN-THINGS))
                 )

            (remove-duplicates THINGS :test #'eq :from-end t)))))))

;;----------------------------------------------;;

(defun sboo-dwim-forward-op (symbol)

  "Return the movement function of the SYMBOL thing.

Inputs:

• SYMBOL — a `symbolp'.

Output:

• a `functionp' or nil.

Example:

• M-: (sboo-dwim-forward-op 'sexp)
    ⇒ #'`forward-sexp'
• M-: (sboo-dwim-forward-op 'acab)
    ⇒ nil

Notes:

a thing is defined as:

• « 'forward-op » symbols — i.e. any `symbolp' with the « 'forward-op » property.
• « forward-* » functions — any `functionp's which starts with “forward-”.

most things have « forward-* » functions:

• M-: (sboo-dwim-forward-op 'sexp)
    ⇒ #'`forward-sexp'
• M-: (intern-soft (format \"forward-sexp\"))
    ⇒ #'`forward-sexp'
• M-: (get 'sexp 'forward-op)
    ⇒ nil

some things have a (differently-named) « 'forward-op » property:

• M-: (sboo-dwim-forward-op 'defun)
    ⇒ #'`end-of-defun'
• M-: (intern-soft (format \"forward-defun\"))
    ⇒ nil
• M-: (get 'defun 'forward-op)
    ⇒ #'`forward-sexp'"

  (or (get symbol 'forward-op)
      (intern-soft (format "forward-%s" symbol))))

;;----------------------------------------------;;
;; Functions: Getters --------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-dwim-get (&key (predicate #'sboo-dwim/textual-string-p)
                              (things    '(line word char))
                              (reader    #'read-string)
                              (prompt    "String: ")
                              )

  "Do-What-I-Mean — get a “valid” string from the user.

Inputs:

• PROMPT    — a `stringp'.

• PREDICATE — a `functionp'.
  PREDICATE defines what a “valid” string will be.
  Takes one `stringp' and gives a `booleanp'.

• THINGS — a `listp' of `symbolp's.
  By default, “thing”s include: ‘symbol’, ‘list’, ‘sexp’, ‘defun’, ‘filename’, ‘url’, ‘email’, ‘word’, ‘sentence’, ‘whitespace’, ‘line’, ‘number’, ‘page’.

• READER — a `functionp'.
  Should take one `stringp' (the prompt). 
  Should give a `stringp' (but may give any `objectp').

Output:

• a `stringp' or nil. try (in order) each of:

    • the region             (if a valid string is highlighted).
    • the current sentence   (if valid).
    • the current line       (if valid).
    • the current word       (if valid).
    • the clipboard contents (if valid).
    • user input             (valid or invalid).

Examples:

• M-: (sboo-dwim-get :predicate #'sboo-dwim/textual-string-p :things '(word char) :prompt \"Number: \" :reader #'read-number)
   ⇒ ...

Links:

• URL `https://docs.microsoft.com/en-us/windows/desktop/dataxchg/standard-clipboard-formats'
• URL `'"

  (let* ((OBJECT

          (or

           ;;-----------------------------------------;;
           ;; Try the current selection:

           (condition-case _

               (when (use-region-p)
                 (filter-buffer-substring (region-beginning) (region-end)))

             (error
              nil)) ;TODO (ERROR-SYMBOL . SIGNAL-DATA)

           ;;-----------------------------------------;;
           ;; Try the current “thing”s (at `point'):

           (condition-case e

               (cl-loop for THING in things
                  for TEXT = (thing-at-point THING :no-text-properties)
                  if (funcall predicate TEXT)
                  collect TEXT)

             ;; “thing”s include: ‘symbol’, ‘list’, ‘sexp’, ‘defun’, ‘filename’, ‘url’, ‘email’, ‘word’, ‘sentence’, ‘whitespace’, ‘line’, ‘number’, ‘page’.

             (error
              (message "%S" e)))  ;TODO (ERROR-SYMBOL . SIGNAL-DATA)

           ;;-----------------------------------------;;
           ;; Try the current contents of the (emacs) clipboard:

           (condition-case _

               (let* ((CLIPBOARD-CONTENTS (car kill-ring))
                      (TEXT-P             (and CLIPBOARD-CONTENTS
                                               (funcall predicate CLIPBOARD-CONTENTS)))
                      )
                 (if TEXT-P
                     (substring-no-properties CLIPBOARD-CONTENTS)))

             (error
              nil))

           ;;-----------------------------------------;;
           ;; Try the current contents of the (system) clipboard:

           (condition-case _                            ;TODO

               (let* ((CLIPBOARD-CONTENTS (if (fboundp #'simpleclip-get-contents)
                                              (simpleclip-get-contents)
                                            nil))
                      (TEXT-P             (and CLIPBOARD-CONTENTS
                                               (funcall predicate CLIPBOARD-CONTENTS)))
                      )
                 (if TEXT-P
                     (substring-no-properties CLIPBOARD-CONTENTS)))

             (error
              nil))

           ;;-----------------------------------------;;
           ;; Finally, prompt the user:

           (condition-case _

               (when (functionp reader)
                 (if (stringp prompt)
                     (funcall reader prompt)
                   (funcall reader)))

             (error
              nil))

           ;;-----------------------------------------;;

           nil)))

    (progn

      (when (called-interactively-p)
        (message "%S" OBJECT))

      OBJECT)))

;; Examples:
;;
;; M-: (sboo-dwim-get :predicate (lambda (s) (let ((n (string-to-number s))) (and (numberp n) (/= n 0)))) :things '(word char) :prompt "Number: " :reader #'read-number)
;;
;;
;; M-: (sboo-dwim-get :predicate (lambda (s) (let ((c (string-to-char s))) (and ( c) ( c)))) :things '(char) :prompt "Character: " :reader #'read-char)
;;
;;
;; M-: (let ((choices '(white blue black red green))) (sboo-dwim-get :predicate (lambda (s) (let ((choice (intern s))) (memq choice choices))) :things '(char word line) :prompt nil :reader (completing-read "Color: " choices))) ; blue
;;
;; M-: (let ((choices '("white" "blue" "black" "red" "green"))) (sboo-dwim-get :predicate (lambda (s) (member s choices)) :things '(char word line) :prompt nil :reader (completing-read "Color: " choices))) ; blue
;;
;; M-: (progn (end-of-line) (let ((xs '(white blue black red green))) (sboo-dwim-get :predicate (lambda (s) (let ((x (intern s))) (memq x xs))) :things '(char word line) :prompt nil :reader (lambda (&optional *PROMPT*) (intern (completing-read *PROMPT* choices)))))) ; blue
;;
;; M-: (progn (end-of-line) (let ((xs '(white blue black red green))) (sboo-dwim-get :predicate (lambda (s) (let ((x (intern s))) (memq x xs))) :things '(char word line) :prompt nil :reader (lambda () (intern (completing-read "Color: " xs)))))) ; blue
;;

;;----------------------------------------------;;

(cl-defun sboo-dwim-get-things-at-point (&key (predicate #'sboo-dwim/textual-string-p) (things '(line word symbol char)))

  "Return all “valid” nearby ‘thing’s (and which ‘thing’ each is).

Inputs:

• PREDICATE — a `functionp', or t.
  PREDICATE defines what “valid” means
  Takes one `stringp' and gives a `booleanp'.
  t means `identity'.

• THINGS — a `listp' of `symbolp's, or t.
  t means “All Things” (via `sboo-dwim-all-things'),
  including: ‘symbol’, ‘list’, ‘sexp’, ‘defun’, ‘filename’, ‘url’, ‘email’, ‘word’, ‘sentence’, ‘whitespace’, ‘line’, ‘number’, ‘page’.

Output:

• an Association-List, a `listp' of `consp's, 
  with `car' `symbolp's and `cdr' `stringp's.

Examples:

• 【M-x sboo-dwim-get-things-at-point】 on 【example▮ text】
• 【M-: (sboo-dwim-get-things-at-point :things t :predicate t)】 on 【example▮ text】

(where ▮ represent the `point'.)"

  (let* ((THINGS    (if (eq t things)    (sboo-dwim-all-things :fast t) things))
         (PREDICATE (if (eq t predicate) #'identity predicate))
         )

    (let* ((VALID-THINGS
            (cl-loop for THING in THINGS
               for TEXT = (thing-at-point THING :no-text-properties)
               if (funcall PREDICATE TEXT)
               collect (cons THING TEXT)))

           (ALIST VALID-THINGS))

      ALIST)))

;;----------------------------------------------;;

(cl-defun sboo-dwim-get-thing-at-point (&key (predicate #'sboo-dwim/textual-string-p) (things '(line word symbol char)))

  "Return a “valid” nearby ‘thing’.

Inputs:

• PREDICATE — a `functionp'.
  PREDICATE defines what “valid” means
  Takes one `stringp' and gives a `booleanp'.

• THINGS — a `listp' of `symbolp's.
  “Things” include: ‘symbol’, ‘list’, ‘sexp’, ‘defun’, ‘filename’, ‘url’, ‘email’, ‘word’, ‘sentence’, ‘whitespace’, ‘line’, ‘number’, ‘page’.

Output:

• a `stringp', or nil."

  (cl-loop for THING in things
     for TEXT = (thing-at-point THING :no-text-properties)
     if (funcall predicate TEXT)
     collect TEXT))

;; e.g. `cl-loop':
;;
;; M-: (cl-loop for THING in '(url char line word symbol) for TEXT = (symbol-name THING) if (funcall (lambda (*S*) (= (length *S*) 4)) TEXT) collect TEXT)
;;   ⇒ ("char" "line" "word")
;;

;;----------------------------------------------;;

(cl-defun sboo-dwim-get-number (&key prompt predicate things reader)

  "Do-What-I-Mean — get a number.

Input:

• PROMPT — an optional `stringp'.

Output:

• a `numberp' or nil.

Related:

• Wraps `sboo-dwim-get'."

  (let* ((PROMPT (or prompt "Number: "))
         )

  (sboo-dwim-get :predicate #'sboo-dwim/numerical-string-p
                 :things    '(number char)
                 :prompt    PROMPT
                 :reader    #'read-number
                 )))

;;----------------------------------------------;;

(cl-defun sboo-dwim-get-character (&key prompt predicate things reader)

  "Do-What-I-Mean — get a character.

Input:

• PROMPT — an optional `stringp'.

Output:

• a `characterp' or nil.

Related:

• Wraps `sboo-dwim-get'."

  (let* ((PROMPT (or prompt "Character: "))
         )

  (sboo-dwim-get :predicate #'sboo-dwim/interesting-character-p
                 :things    '(char)
                 :prompt    PROMPT
                 :reader    #'read-char
                 )))

;;----------------------------------------------;;

(cl-defun sboo-dwim-get-enum-symbol (xs &key prompt predicate things reader)

  "Do-What-I-Mean — get a character.

Input:

• XS     — a `listp' of `symbolp's.
• PROMPT — an optional `stringp'.

Output:

• a `symbolp' or `nil'.
  a `member' of XS.

Related:

• Wraps `sboo-dwim-get'."

  (let* ((THINGS    (or things
                        '(char word line)))

         (PROMPT    (or prompt
                        nil))
         (READER    (or reader
                        (lambda ()
                          (intern
                           (completing-read PROMPT choices)))))

         (MEMBER?   (lambda (s)
                      (let ((x (intern s)))
                        (memq x xs))))
         (PREDICATE (or (if (functionp predicate) predicate nil)
                        MEMBER?))
         )

  (sboo-dwim-get :predicate PREDICATE
                 :things    THINGS
                 :reader    READER
                 :prompt    nil
                 )))

;; M-: (sboo-dwim-get-enum-symbol '(white blue black red green) :prompt "Color: ") ; blue

;;----------------------------------------------;;

(cl-defun sboo-dwim-get-url (&key prompt predicate things reader)

  "Do-What-I-Mean — get a URL.

Input:

• PROMPT — an optional `stringp'.

Output:

• a `stringp' or nil.

Related:

• Wraps `sboo-dwim-get'."

  (let* ((PROMPT (or prompt "Url: "))
         )

  (sboo-dwim-get :predicate nil
                 :things    '(url)
                 :prompt    PROMPT
                 :reader    #'read-string
                 )))

;;----------------------------------------------;;

(cl-defun sboo-dwim-get-file (&key prompt predicate things reader)

  "Do-What-I-Mean — get a filepath (regular or directory).

Input:

• PROMPT — an optional `stringp'.

Output:

• a `stringp' or nil.

Related:

• Wraps `sboo-dwim-get'."

  (let* ((PROMPT (or prompt "File: "))
         )

  (sboo-dwim-get :predicate nil
                 :things    '(filename)
                 :prompt    PROMPT
                 :reader    #'read-file-name
                 )))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;; 

;; `cl-loop':
;; 
;;    (cl-loop for x below 10
;;             if (cl-oddp x)
;;               collect x into odds
;;             else
;;               collect x into evens
;;             finally return `(:odd ,odds :even ,evens))

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-dwim)

;;; sboo-dwim.el ends here