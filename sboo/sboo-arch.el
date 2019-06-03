;;; sboo-arch.el --- sboosali's architecture-specific config -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 03 Jun 2019
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

;; Personal architecture-specific configuration.
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile
  (require 'pcase))

;;----------------------------------------------;;
;; Conditions ----------------------------------;;
;;----------------------------------------------;;

(defun sboo-arch-run-system ()

  "The processor-architecture, at run-time.

Output:

• a `symbolp'. One of:

    • 'intel64 — 64-bit Intel.
    • 'arm64   — 64-bit ARM.

Uses:

• program `arch'
• program `uname'

Implementation...

e.g. `intel64':

    $ arch
    x86_64

    $ uname -m
    x86_64
    
e.g. `arm64':

    $ arch
    aarch64

    $ uname -m
    aarch64

Links:

• URL `'"

  (let* (($ARCH  (string-trim (shell-command-to-string "arch")))
         ($UNAME (string-trim (shell-command-to-string "uname -m")))
         )

    (pcase $ARCH

      ("x86_64"  'intel64)
      ("aarch64" 'aarch64)
      ("" (pcase $UNAME

            ("x86_64"  'intel64)
            ("aarch64" 'aarch64)
            ("" nil))
       (_  nil))

    (_ nil))))

;; M-: (sboo-arch-run-system)

;;----------------------------------------------;;

(defvar sboo-arch-current-system

  (or (sboo-arch-run-system) nil) ;TODO (sboo-arch-build-system))

  "Current (runtime) operating-system (via `sboo-arch-run-system').")

;;----------------------------------------------;;
;; “Re-Exports” --------------------------------;;
;;----------------------------------------------;;

(pcase sboo-arch-current-system

  ('intel64 (require 'sboo-arch-intel-64))
  ('arm64   (require 'sboo-arch-arm-64))

  (_ ()))

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-arch)

;;; sboo-arch.el ends here