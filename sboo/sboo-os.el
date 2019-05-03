;;; sboo-os.el --- -*- lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
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
  (require 'cl-lib))

(progn
  (require 'pcase))

;;----------------------------------------------;;

(defmacro sboo-os-build-system ()

  "The operating system, at build-time.

Output:

• a `symbolp'. One of:

    • 'linux   — for Linux (and/or X11).
    • 'windows — for Windows (and/or WIN32).
    • 'macos   — for OSX (and/or Cocoa).

Uses:

• `system-type'"

  (pcase system-type

    ('gnu/linux               (quote 'linux))
    ((or 'windows-nt 'ms-dos) (quote 'windows))
    ('darwin                  (quote 'macos))
    (_                        (quote nil)))

;; M-: (sboo-os-build-system)

;;----------------------------------------------;;

(defun sboo-os-run-system ()

  "The operating system, at run-time.

Output:

• a `symbolp'. One of:

    • 'linux   — for Linux (and/or X11).
    • 'windows — for Windows (and/or WIN32).
    • 'macos   — for OSX (and/or Cocoa).

Uses:

• `system-type'"

  (pcase system-type

    ('gnu/linux               'linux)
    ((or 'windows-nt 'ms-dos) 'windows)
    ('darwin                  'macos)
    (_                        nil)))

;; M-: (sboo-os-run-system)

;;----------------------------------------------;;

(defvar sboo-os-current-system

  (sboo-os-run-system)

  "Current (runtime) operating-system (via `window-system').")

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-os)

;;; sboo-os.el ends here