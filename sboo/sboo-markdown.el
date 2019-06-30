;;; sboo-markdown.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 28 Jun 2019
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

;; Personal `markdown-mode' Utilities & Configuration.
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
;;; Variables ----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;;; Commands -----------------------------------;;
;;----------------------------------------------;;

(defalias 'sboo-markdown-backward-h1 #'markdown-backward-page)
(defalias 'sboo-markdown-forward-h1  #'markdown-forward-page)

;;==============================================;;

(defun sboo-markdown-mark-h1 ()

  "`mark' the current toplevel header."

  (interactive)

  (let* ((COUNT +1))

    (sboo-markdown-backward-h1)

    (markdown-mark-subtree)

    COUNT))

;;----------------------------------------------;;

(defun sboo-markdown-goto-h1 (&optional default)

  "Goto a toplevel header (i.e. « # » or « <h1> »), by name."

  (interactive)

  (let* ()

    (TODO)

    ()))

;;----------------------------------------------;;
;;; Functions ----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-markdown)

;;; sboo-markdown.el ends here