;;; sboo-dev-chromebook.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 06 Jun 2019
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

;; .
;; 
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
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-translate-function-keys-under-control ()

  "Translate « <f1> » as « C-1 », « <f11> » as « C-0 1 », etc."

  (progn

    (define-key key-translation-map (kbd "C-1")   (kbd "<f1>"))
    (define-key key-translation-map (kbd "C-2")   (kbd "<f2>"))
    (define-key key-translation-map (kbd "C-3")   (kbd "<f3>"))
    (define-key key-translation-map (kbd "C-4")   (kbd "<f4>"))
    (define-key key-translation-map (kbd "C-5")   (kbd "<f5>"))
    (define-key key-translation-map (kbd "C-6")   (kbd "<f6>"))
    (define-key key-translation-map (kbd "C-7")   (kbd "<f7>"))
    (define-key key-translation-map (kbd "C-8")   (kbd "<f8>"))
    (define-key key-translation-map (kbd "C-9")   (kbd "<f9>"))
    (define-key key-translation-map (kbd "C-0 1") (kbd "<f11>"))
    (define-key key-translation-map (kbd "C-0 2") (kbd "<f12>"))

    ()))

;;----------------------------------------------;;
;; Effects -------------------------------------;;
;;----------------------------------------------;;

(sboo-translate-function-keys-under-control)

;; ^ Chromebook has no F-Keys.

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-dev-chromebook)

;;; sboo-dev-chromebook.el ends here