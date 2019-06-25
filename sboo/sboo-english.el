;;; sboo-english.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 16 May 2019
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

;; English Dictionary.
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
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-english nil

  "Personal English dictionary and spell-checking."

  :prefix 'sboo
  :group 'sboo)

;;----------------------------------------------;;

(defcustom sboo-english-dictionary-file

  "/etc/dictionaries-common/words"

  "Filepath to an English dictionary.

File must have one word per line.

a `stringp'."

  :type '(file :tag "File"
               :must-match t)

  :safe #'stringp
  :group 'sboo)

;;----------------------------------------------;;

(defcustom sboo-english-extra-words

  '(
    "lich"
   )

  "Personal dictionary.

Extra words to extend `sboo-english-words' with.

a `listp' of `stringp's."

  :type '(repeat (string :tag "Word"))

  :safe #'listp
  :group 'sboo)

;;----------------------------------------------;;

(defvar sboo-english-words nil

  "English Dictionary.

Read from file `sboo-english-dictionary-file'.

a `listp' of `stringp's.")

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-english-words (&optional force)

  "Accessor for variable `sboo-english-words'.

Inputs:

• FORCE — a `booleanp'.
  whether to force reloading the dictionary (via `sboo-load-dictionary-words')
  and reinitializing the cache variable `sboo-english-words'.

Output:

• a `listp' of `stringp's."

  (if (and sboo-english-words (not force))
      sboo-english-words

    (let* ((WORDS (append sboo-english-extra-words
                          (sboo-load-dictionary-words)))
           )

      (setq sboo-english-words WORDS)

      WORDS)))

;;----------------------------------------------;;

(defun sboo-load-dictionary-words ()

  "Load `sboo-english-dictionary-file'.

Output:

• a `listp' of `stringp's."

  (split-string (with-temp-buffer
                  (insert-file-contents-literally sboo-english-dictionary-file)
                  (buffer-string))
                "\n"))

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-insert-english-word (&optional force)

  "Insert a known English word (with completion).

Related:

• `sboo-english-words'"

  (interactive
   (let ((FORCE (if current-prefix-arg t nil))
         )
     (list FORCE)))

  (let* ((PROMPT        (format "%s: " "English Word"))
         (REQUIRE-MATCH nil)
         (CANDIDATES    (sboo-english-words))

         (COMPLETION (completing-read PROMPT CANDIDATES nil REQUIRE-MATCH))
         )

    (let* ((STRING (string-trim COMPLETION))
           )

      (insert STRING))))

;;----------------------------------------------;;

(defun sboo-english-words-completion-at-point ()

  "`completion-at-point' for `sboo-english-words'.

Links:

• URL `https://emacs.stackexchange.com/questions/15276/how-do-i-write-a-simple-completion-at-point-functions-function'"

  (interactive)

  (when-let* ((BOUNDS (bounds-of-thing-at-point 'word))
              (BEG (car BOUNDS))
              (END (cdr BOUNDS))
              )

    (list BEG
          END
          (sboo-english-words)

          :exclusive 'no

          :company-docsig     #'sboo-english-company-docsig
          :company-doc-buffer #'sboo-english-company-doc-buffer
          :company-location   #'sboo-english-company-location
          )))

;;----------------------------------------------;;
;; Functions: `company' ------------------------;;
;;----------------------------------------------;;

(with-eval-after-load 'company

  ;;--------------------------;;

  (defun sboo-english-company-docsig (candidate)

    "« :company-docsig » for `sboo-english-words-completion-at-point'.

Inputs:

• CANDIDATE — a `stringp'.

Output:

• a `stringp'.
  TODO return the definition of the word."

    candidate)

  ;;--------------------------;;

  (defun sboo-english-company-doc-buffer (candidate)

    "« :company-doc-buffer » for `sboo-english-words-completion-at-point'.

Inputs:

• CANDIDATE — a `stringp'.

Output:

• a `bufferp'."

    (let* ((STRING (format "« %s » is defined in « %s »'" candidate sboo-english-dictionary-file))
           )

      (company-doc-buffer STRING)))

  ;;--------------------------;;

  (defun sboo-english-company-location (candidate)

    "« :company-location » for `sboo-english-words-completion-at-point'.

Inputs:

• CANDIDATE — a `stringp'.

Output:

• a `consp' whose:

    • `car' — is a `bufferp'.
    • `cdr' — is an `integerp'.
       a position with that buffer."

    (with-current-buffer (find-file-noselect sboo-english-dictionary-file)

      (goto-char (point-min))

      (let* ((BUFFER (current-buffer))

             (SEARCH-BOUND   nil)
             (SEARCH-NOERROR t)
             )

        (cons BUFFER
              (let ((case-fold-search nil)) ; case-sensitive
                (search-forward candidate SEARCH-BOUND SEARCH-NOERROR))))))

  ;;--------------------------;;

  'company)

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-english)

;;; sboo-english.el ends here