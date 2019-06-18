;;; sboo-package.el --- My ‘package.el’ config -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 17 Jun 2019
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

;; Personal ‘package.el’ configuration.
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

(defun sboo-ensure-package (package)

  "`package-install' PACKAGE (unless already present) and `require' it (unless already loaded).

=== Usage ===

e.g.:

    (sboo-ensure-package 'magit)

=== Links ===

URL `https://www.draketo.de/light/english/emacs/babcore'"

  ; try to activate the package with at least version 0.
  (package-activate package '(0))
  ; try to just require the package. Maybe the user has it in his local config

  (condition-case nil
      (require package)
    ; if we cannot require it, it does not exist, yet. So install it.
    (error (progn
             (package-install package)
             (require package)))))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-package)

;;; sboo-package.el ends here