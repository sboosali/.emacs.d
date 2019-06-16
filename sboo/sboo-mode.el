;;; sboo-mode.el --- Personal globalized-minor-mode -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 11 Jun 2019
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

;; Global Minor Mode for personal keybindings.
;; 
;; Toggle via:
;; 
;; • Run `sboo-global-mode' — 
;; • Click on `sboo-global-mode-toolbar-entry' — 
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
;; Images --------------------------------------;;
;;----------------------------------------------;;

(defimage sboo-mode-image

  ((:type svg :data ""))

  "`sboo-mode' logo.")

;;----------------------------------------------;;
;; Mode ----------------------------------------;;
;;----------------------------------------------;;

(defvar sboo-mode-map

  (let ((MAP (make-sparse-keymap)))

 ;; (define-key MAP (kbd "q") #'turn-off-sboo-mode)

    MAP)

  "`keymapp' for `sboo-mode'.")

;;----------------------------------------------;;

(define-minor-mode sboo-mode

    "Minor Mode for personal keybindings.

=== Keybindings ===

\\{sboo-mode-map}"

  :lighter " SBoo"

  :keymap sboo-mode-map

  :group 'sboo

  :init-value t

  (if (bound-and-true-p sboo-mode)

      ()

    ()))

;;----------------------------------------------;;

(defun turn-on-sboo-mode ()
  "Enable `sboo-mode'."
  (interactive)
  (sboo-mode +1))

;;----------------------------------------------;;

(defun turn-off-sboo-mode ()
  "Disable `sboo-mode'."
  (interactive)
  (sboo-mode -1))

;;==============================================;;

;;(define-globalized-minor-mode sboo-global-mode sboo-mode sboo-mode)

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-mode)

;;; sboo-mode.el ends here

;; Local Variables:
;; End:
