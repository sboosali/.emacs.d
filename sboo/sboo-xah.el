;;; sboo-xah.el --- Xah Lee functions
;;; -*- lexical-binding: t; -*-

;;==============================================;;
;; Copyright (C) GPL-3.0-or-later 2019 Spiros Boosalis

;; Author: Spiros Boosalis <samboosalis@gmail.com>
;; Author: Xah Lee

;; This file is NOT part of GNU Emacs.

;;==============================================;;
;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;==============================================;;
;;; Commentary:

;; Functions written by Xah Lee,
;; and modified by Spiros Boosalis.
;;

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;;; Requirements -------------------------------;;
;;----------------------------------------------;;

(require 'cl-lib)
(require 'pcase)
(require 'seq)
(require 'subr-x)

;;----------------------------------------------;;
;;; Commands -----------------------------------;;
;;----------------------------------------------;;

(defun xah-forward-block (&optional n)
 "Move cursor beginning of next text block.
A text block is separated by blank lines.
This command similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
 (interactive "p")
 (let ((n (if (null n) 1 n)))
   (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" n)))

;;----------------------------------------------;;

(defun xah-backward-block (&optional n)
 "Move cursor to previous text block.
See: `xah-forward-block'
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
 (interactive "p")
 (let ((n (if (null n) 1 n))
       ($i 1))
   (while (<= $i n)
     (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
         (progn (skip-chars-backward "\n\t "))
       (progn (goto-char (point-min))
              (setq $i n)))
     (setq $i (1+ $i)))))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun xah-escape-quotes-dwim (@begin @end)
  "Replace 「\"」 by 「\\\"」 in current line or text selection.
See also: `xah-unescape-quotes-dwim'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2017-01-11"

  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))

  (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (goto-char (point-min))
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" :fixedcase :literal)))))

;;----------------------------------------------;;

(defun xah-unescape-quotes-dwim (@begin @end)
  "Replace  「\\\"」 by 「\"」 in current line or text selection.
See also: `xah-escape-quotes-dwim'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'

Version 2017-01-11"

  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))

  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" :fixedcase :literal)))))

;;----------------------------------------------;;

(defun xah-search-current-word ()

  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)

  (let ( $p1 $p2 )

    (if (use-region-p)

        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))

      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9") ;TODO dont use region if only whitespace
        (setq $p1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq $p2 (point))))

    (setq mark-active nil)

    (when (< $p1 (point))
      (goto-char $p1))

    (isearch-mode t)

    (isearch-yank-string (buffer-substring-no-properties $p1 $p2))))

;;==============================================;;

(provide 'sboo-xah)

;; Local Variables:
;; End:

;;; sboo-xah.el ends here
