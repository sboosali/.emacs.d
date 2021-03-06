;;; sboo-flycheck.el --- -*- lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 06 May 2019
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

;; Personal `flycheck' configuration.
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

(eval-when-compile
  (require 'rx)
  (require 'cl-lib))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-flycheck-display-buffer

  `( ,(rx bos "*Flycheck errors*" eos)
     (display-buffer-reuse-window display-buffer-in-side-window)
     (side            . bottom)
     (reusable-frames . visible)
     (window-height   . 0.33)
   )

  "Display Rule for `flycheck' buffrers.

Display Rule which tells Emacs to always display the error list at the bottom side of the frame,
 occupying a third of the entire height of the frame.

Links:

• URL `http://www.flycheck.org/en/latest/user/error-list.html#tune-error-list-display'.")

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun sboo-flycheck ()

  "Display the « Flycheck Errors » buffer.

Calls:

•  `flycheck-list-errors'.
•  `delete-other-windows'.

Notes:

• « Flycheck Errors » has major mode `flycheck-error-list-mode'.

• With `sboo-flycheck-display-buffer' in `display-buffer-alist', 
  the `flycheck-error-list-mode' buffer is shown on the Bottom Window."

  (interactive)

  (delete-other-windows)
  (flycheck-list-errors))

;; ^ NOTE
;; 
;;   • `flycheck-list-errors' calls `display-buffer'.
;; 

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-flycheck)

;;; sboo-flycheck.el ends here