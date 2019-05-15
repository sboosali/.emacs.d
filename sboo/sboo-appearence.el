;;; sboo-appearence.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 15 May 2019
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

;; Appearence configuration and utilities, including Theming.
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(progn
  (require 'color))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun sboo-current-screen-dimensions ()

  "Return the current monitor's dimensions.

Output:

• a `listp'.
  a Keyword Property-List (see « Examples »).

Examples:

• M-: (sboo-current-screen-dimensions)
    ⇒ '(:width 3200 :height 1800)"

  (list :width  (display-pixel-width)
        :height (display-pixel-height)
        ))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;; 
;; 
;; 

;; ^ DOCS `read-color':
;;
;; « (read-color &optional PROMPT CONVERT ALLOW-EMPTY DISPLAY) »
;;
;; > This function reads a string that is a color specification, either the color's name or an RGB hex value such as #RRRGGGBBB. It prompts with PROMPT (default: "Color (name or #RGB triplet):") and provides completion for color names, but not for hex RGB values. In addition to names of standard colors, completion candidates include the foreground and background colors at point.
;; > 
;; > However, when called interactively or if the optional argument convert is non-nil, it converts any input color name into the corresponding RGB value string and instead returns that. This function requires a valid color specification to be input. Empty color names are allowed when allow-empty is non-nil and the user enters null input.
;; > 
;; > Interactively, or when display is non-nil, the return value is also displayed in the echo area.
;; > 
;;

;; `use-package':
;;
;;   :custom-face
;;   (eruby-standard-face ((t (:slant italic)))))
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

(provide 'sboo-appearence)

;;; sboo-appearence.el ends here