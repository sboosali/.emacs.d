;;; sboo-ui.el --- -*- lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 03 May 2019
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

;; Builtins:

(eval-when-compile
  (require 'pcase)
  (require 'cl-lib)
  )

(progn
  
  )

;;----------------------------------------------;;
;; Conditions ----------------------------------;;
;;----------------------------------------------;;

(defmacro sboo-ui-build-system ()

  "The operating system, at build-time.

Output:

• a `symbolp'. One of:

    • 'cli — “Command-Line Interface”  — (i.e. launched by « $ emacs -nw »).
    • 'gui — “Grahical User Interface” — (i.e. launched by « $ emacs »).

Uses:

• `window-system'"

  (pcase window-system

    ('nil (quote 'cli))
    (_    (quote 'gui))))

;; M-: (sboo-ui-build-system)

;;----------------------------------------------;;

(defun sboo-ui-run-system ()

  "The operating system, at run-time.

Output:

• a `symbolp'. One of:

    • 'cli — “Command-Line Interface”  — (i.e. launched by « $ emacs -nw »).
    • 'gui — “Grahical User Interface” — (i.e. launched by « $ emacs »).

Uses:

• `window-system'"

  (pcase window-system

    ('nil 'cli)
    (_    'gui)))

;; M-: (sboo-ui-run-system)

;;----------------------------------------------;;

(defvar sboo-ui-current-system

  (or (sboo-ui-run-system) (sboo-ui-build-system))

  "Current (runtime) operating-system (via `sboo-ui-run-system').")

;;----------------------------------------------;;
;; “Re-Exports” --------------------------------;;
;;----------------------------------------------;;

(pcase sboo-ui-current-system

  ('cli (require 'sboo-ui-terminal))
  ('gui (require 'sboo-ui-graphical))

  (_ ()))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-ui)

;;; sboo-ui.el ends here