;;; sboo-os-windows.el --- -*- lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
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

;; « emacs.exe » (on Windows):
;;
;; • needs `sboo-windows-maximize-frame'
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-windows-tramp-method

  "plink"

  "Windows-specific `tramp-default-method'.

Windows lacks the program `ssh'.")

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun sboo-windows-configure-modifiers ()

  "Configure a Windows keyboard's Modifier Keys (for Windows).

Effects:

• Set Left&Right « ❖ » (Window) to « super ».
• Set « ▤ » (Menu/App) to « hyper ».

A (standard) Windows keyboard has these Modifier Keys:

• « ❖ » — the (Left and Right) “Window” key(s).
• « ▤ » — the “Menu” (a.k.a. “App”) key.

Links:

• URL `http://ergoemacs.org/emacs/emacs_hyper_super_keys.html'"

  (progn

    (setq w32-pass-lwindow-to-system nil)
    (setq w32-pass-rwindow-to-system nil)
    (setq w32-pass-apps-to-system    nil)

    (setq w32-lwindow-modifier 'super) ; « lwindow » means the “Left Windows” key.
    (setq w32-rwindow-modifier 'super) ; « rwindow » means the “Right Windows” key.
    (setq w32-apps-modifier    'hyper) ; « apps » means the “Menu/App” key.

    ()))

;;----------------------------------------------;;

(defun sboo-windows-maximize-frame ()

  "Maximize the `selected-frame'.

Links:

• URL `https://michael.englehorn.com/config.html'

Related:

• `w32-send-sys-command'"

  (interactive)

  (w32-send-sys-command 61488))

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-os-windows)

;;; sboo-os-windows.el ends here