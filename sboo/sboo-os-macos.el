;;; sboo-os-macos.el --- -*- lexical-binding: t -*-

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

;; « Emacs.app » (on MacOS):
;;
;; • needs `exec-path-from-shell'.
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins (internal):

;;----------------------------------------------;;

;; packages (external):

(eval-when-compile 
  (require 'use-package))

;;----------------------------------------------;;
;; Configuration -------------------------------;;
;;----------------------------------------------;;

(use-package exec-path-from-shell

  :config

  (exec-path-from-shell-initialize))

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun sboo-macos-configure-modifiers ()

  "Configure a Apple keyboard's Modifier Keys (on MacOS).

Effects:

• Set « ⌘ » (Command) to « meta ».
• Set Left « ⌥ » (Option) to « super ».
• Set Right(???) « ⌥ » (Option) to to « hyper ».

A (standard) Apple keyboard has these Modifier Keys:

• « ⌘ » — the (Left and Right) “Apple” keys.
• « ⌥ » — the “Option” key.

Links:

• URL `http://ergoemacs.org/emacs/emacs_hyper_super_keys.html'"

  (progn

    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier  'super)
    (setq mac-control-modifier 'control)

    ;;(setq ns-function-modifier 'hyper)
    ()))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; `exec-path-from-shell'...
;;
;; `exec-path-from-shell-initialize' calls `exec-path-from-shell-copy-env':
;;
;; • (exec-path-from-shell-copy-env "PATH")
;; • (exec-path-from-shell-copy-env "MANPATH")
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-os-macos)

;;; sboo-os-macos.el ends here