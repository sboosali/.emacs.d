;;; sboo-macros.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 08 May 2019
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

;; Personal `defmacro's.
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile 
  (require 'pcase)
  (require 'cl-lib))

;;----------------------------------------------;;
;; Types ---------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Macros --------------------------------------;;
;;----------------------------------------------;;

(cl-defmacro sboo-defun (&key )

  "`defun' with argument validation.

Example:

• M-: (pp-macroexpand-expression (sboo-defun sboo-add ((x \"A number\" :validate numberp) (y \"A number\" :validate numberp :default 2)) \"Add X and Y.\" (+ x y)))
    ⇒ TODO

Links:

• URL `http://kitchingroup.cheme.cmu.edu/blog/2017/03/22/A-better-defun-for-emacs-lisp/'
• URL `https://github.com/jkitchin/scimax/blob/master/scimax-macros.el'"

  (let* ()

    `(cl-defun

         )))

;; e.g.:
;;
;; (sboo-defun sboo-add ((x "A number" :validate numberp)
;;                       (y "A number" :validate numberp :default 2))
;;   "Add X and Y."
;;   (+ x y))

;;----------------------------------------------;;

(defmacro defn (fname args &optional docstring &rest body)
  "Macro to define a function with improved argument documentation.
FNAME is a symbol for the function name.
ARGS is a list of arguments. Each argument can be either a symbol
or a list of the form (arg-symbol arg-docstring options) where
options is a plist style of options that include:

:default value
:validate function (the function takes one argument, and should
return t if the argument is valid.
:rest (this indicates the rest of the arguments go into this
variable, and it has to be last)

The function docstring is built up from that information.

Default values will automatically be set in the body, and
validation code will be automatically generated if the option is
present.

DOCSTRING is an optional string for the overall purpose of the
function. The argument docstrings will be appended onto this.

BODY is a form for the function."
  (if (not (stringp docstring))
      (setq body docstring
	    docstring "No documentation provided."))
  (let* (_ds
	 arg-options
	 ;; build up the docstring.
	 (ds (concat
	      (or docstring "No docstring defined.")
	      "\n"
	      (mapconcat
	       'identity
	       (loop for arg in args
		     collect
		     (cond
		      ((listp arg)
		       (setq arg-options (if (stringp (nth 1 arg))
					     (cddr arg)
					   (cdr arg)))
		       (format "%s : %s%s%s"
			       (upcase (symbol-name (car arg)))
			       (if (stringp (nth 1 arg)) (nth 1 arg) "No documentation")
			       (if (plist-get arg-options :default)
				   (format " (default = %s)"
					   (plist-get arg-options :default))
				 "")
			       (if (plist-get arg-options :validate)
				   (format " (valid = %s)"
					   (plist-get arg-options :validate))
				 "")))
		      ;; this is a standalone symbol
		      (t
		       (format "%s : No documentation"
			       (upcase (symbol-name arg))))))
	       "\n")
	      "\n"))
	 ;; These are the args to go in the function definition
	 (newargs (loop for arg in args
			append
			(cond
			 ((listp arg)
			  (cond
			   ((plist-get (cddr arg) :default)
			    `(&optional ,(car arg)))
			   ((member  :rest arg)
			    `(&rest ,(car arg)))
			   (t
			    (list (car arg)))))
			 (t
			  (list arg)))))
	 ;; This is the code to set default values
	 (defaults (delq nil
			 (loop for arg in args
			       collect
			       (when (and (listp arg) (plist-get (cddr arg) :default))
				 `(when (null ,(car arg))
				    (setq ,(car arg) ,(plist-get (cddr arg) :default)))))))
	 ;; This is the code to validate arguments
	 (validate (delq nil
			 (loop for i from 0 for arg in args
			       collect
			       (when (and (listp arg) (plist-get
						       (delq :rest (cddr arg)) :validate))
				 `(unless (funcall ',(plist-get
						      (delq :rest (cddr arg))
						      :validate)
						   ,(car arg))
				    (error "In (%s %s) Expected %s to pass %S. Got %S"
					   ,(symbol-name fname) ,(format "%s" newargs)
					   ,(symbol-name (car arg))
					   ',(plist-get (delq :rest (cddr arg)) :validate)
					   ,(car arg)))))))
	 (f `(defun ,fname (,@newargs)
	       ,(or ds "No docstring defined ;(."))))
    (when defaults (setq f (append f `((progn ,@defaults)))))
    (when validate (setq f (append f `((progn ,@validate)))))
    (setq f (append f `,@body))))

;;TODO error message idiomas:
;; should state what is wrong (“File does not exist”), not how
;; things ought to be (“File must exist”).  The convention in Emacs Lisp is
;; that error messages should start with a capital letter, but should not
;; end with any sort of punctuation.

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
(provide 'sboo-macros)