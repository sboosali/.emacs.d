;;; sboo-path.el --- (Personal) filepaths aliases -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: __KEYWORDS__
;; Created: 01 May 2019

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

;; Aliases for frequently-accessed filepaths.
;;
;; Commands:
;;
;; • `xxx-feature-xxx-*'
;;
;; Variables:
;;
;; • `xxx-feature-xxx-*'
;;
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(eval-when-compile
  (require 'rx)
  (require 'cl-lib)
  ())

;;----------------------------------------------;;

(progn
  (require 'subr-x)
  (require 'pcase)
  (require 'seq)
  ())

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defcustom sboo-paths-alist

  '(
    ( . "")
    ( . "")
    ( . "")
    ( . "")
    ( . "")
    ( . "")
   )

  ".

Aliases (`symbolp's) for filepaths (`stringp's)."

  :type '(alist :key-type   (symbol :tag "Alias")
                :value-type (choice (const nil)
                                    (string :tag "Filepath")))

  :safe #'lisp
  :group 'sboo)

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-path)

;; Local Variables:
;; End:

;;; sboo-path.el ends here