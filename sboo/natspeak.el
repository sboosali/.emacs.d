;;; natspeak.el --- “Dragon NaturallySpeaking” minor mode -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/natspeak.el
;; Keywords: Accessibility
;; Created: 16 Jun 2019
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

;; Global minor mode for dictation via “Dragon NaturallySpeaking”.
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
;; Constants -----------------------------------;;
;;----------------------------------------------;;

(defconst natspeak-dns-version "15"

  "Version of “Dragon NaturallySpeaking”.")

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup natspeak nil

  "Customize “Dragon NaturallySpeaking”."

  :prefix 'natspeak
  :group 'accessibility)

;;==============================================;;

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun natspeak-get-frame-visible-text (&optional frame)
  ""

  (let ((FRAME (or frame (selected-frame))))

    (let* ((SCREEN-WINDOWS
            (window-list))
           )

      (mapcar #'natspeak-get-window-visible-text SCREEN-WINDOWS))))

;; M-: (natspeak-get-frame-visible-text)

;;----------------------------------------------;;

(defun natspeak-get-buffer-visible-text (&optional buffer)
  ""

  (let* ((BUFFER (or buffer (current-buffer)))
         (WINDOW (get-buffer-window BUFFER))
         )

    (natspeak-get-window-visible-text WINDOW)))

;; M-: (natspeak-get-buffer-visible-text)

;;----------------------------------------------;;

(defun natspeak-get-window-visible-text (&optional window)
  ""

  (let ((WINDOW (or window (selected-window))))

    (let* ((FIRST-SCREEN-LINE-POINT
            (save-excursion
              (with-selected-window WINDOW
                (move-to-window-line 0)
                (beginning-of-line)
                (point))))

           (LAST-SCREEN-LINE-POINT
            (save-excursion
              (with-selected-window WINDOW
                (move-to-window-line -1)
                (end-of-line)
                (point))))

           (SCREEN-REGION
            (buffer-substring FIRST-SCREEN-LINE-POINT LAST-SCREEN-LINE-POINT))
           )

      SCREEN-REGION)))

;; M-: (natspeak-get-window-visible-text)

;;----------------------------------------------;;
;; Mode ----------------------------------------;;
;;----------------------------------------------;;

(defvar natspeak-mode-map

  (let ((MAP (make-sparse-keymap)))

    (define-key MAP (kbd "q") #'turn-off-natspeak-mode)

    MAP)

  "`keymapp' for `natspeak-mode'.")

;;----------------------------------------------;;

(define-minor-mode natspeak-mode-map

    "Minor Mode for dictation.

=== Keybindings ===

\\{natspeak-mode-map}"

  :lighter " Natspeak"

  :keymap natspeak-mode-map

  :group 'natspeak

  :init-value t

  (if (bound-and-true-p natspeak-mode)

      (progn
        )

    (progn
      )))

;;----------------------------------------------;;

(defun turn-on-natspeak-mode ()
  "Enable `natspeak-mode'."
  (interactive)
  (natspeak-mode +1))

;;----------------------------------------------;;

(defun turn-off-natspeak-mode ()
  "Disable `natspeak-mode'."
  (interactive)
  (natspeak-mode -1))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'natspeak)

;;; natspeak.el ends here