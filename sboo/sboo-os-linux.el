;;; sboo-os-linux.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
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

;; « emacs » (on Linux):
;;
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

(progn
  (require 'seq))


;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun sboo-linux-configure-modifiers ()

  "Configure a Windows keyboard's Modifier Keys (for Linux).

Effects:

• no effects.

A (standard) Windows keyboard has these Modifier Keys:

• « ❖ » — the (Left and Right) “Window” key(s).
• « ▤ » — the “Menu” (a.k.a. “App”) key.

Links:

• URL `http://ergoemacs.org/emacs/emacs_hyper_super_keys.html'"

  (progn
    ()))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; on Linux:
;;
;; ❖ (the Window key), is Super (by default).
;; Super and Hyper cannot be defined within Emacs.
;; 
;; “For example, in Ubuntu 11.04, it's under 〖System ▸ Preferences ▸ keyboard〗 then “Layout” tap, “Options…” button.”
;; 

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-os-linux)

;;; sboo-os-linux.el ends here