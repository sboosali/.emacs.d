;;; -*- lexical-binding: t; -*-
;;; haddock-mode.el --- Major mode for Haddock docstrings

;;----------------------------------------------;;

;; Copyright (C) 2019- Sam Boosalis and haddock-mode
;; Copyright (C) 2007-2017 Jason R. Blevins and markdown-mode

;; Author: Sam Boosalis <samboosalis@gmail.com>
;; Maintainer: Sam Boosalis <samboosalis@gmail.com>
;; Created: 2019
;; Version: 0.0
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; Keywords: Haskell, Haddock
;; URL: https://github.com/sboosali/haddock-mode/

;;----------------------------------------------;;

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;----------------------------------------------;;

;;; Commentary:

;; 

;;----------------------------------------------;;


;;; Code:

;;----------------------------------------------;;

(require 'easymenu)
(require 'outline)
(require 'thingatpt)
(require 'cl-lib)
(require 'url-parse)
(require 'button)
(require 'color)
(require 'rx)





;;----------------------------------------------;;
;;; Variables ----------------------------------;;
;;----------------------------------------------;;

(defgroup haddock nil

  "Edit Haskell comments in the Haddock format"

  :link '(url-link :tag "GitHub" "https://github.com/sboosali/haddock-mode")

  :prefix "haddock-"
  :group 'haskell)

;;==============================================;;

(defcustom haddock-command "haddock"

  "Command to run haddock."

  :type '(choice (string :tag "Shell command") function)
  :group 'haddock)


;;----------------------------------------------;;
;;; Font Lock ----------------------------------;;
;;----------------------------------------------;;

(require 'font-lock)

;;==============================================;;

(defgroup haddock-faces nil
  "Faces used in Haddock Mode"
  :group 'haddock
  :group 'faces)

;;==============================================;;

(defface haddock-italic-face
  '((t (:inherit italic)))
  "Face for italic text."
  :group 'haddock-faces)

;;----------------------------------------------;;

(defface haddock-bold-face
  '((t (:inherit bold)))
  "Face for bold text."
  :group 'haddock-faces)

;;----------------------------------------------;;

(defface haddock-inline-code-face
  '((t (:inherit (haddock-code-face font-lock-constant-face))))
  "Face for inline code."
  :group 'haddock-faces)

;;==============================================;;
(provide 'haddock-mode)