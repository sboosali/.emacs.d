;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for `keywords'.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'keywords)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlight long lines:

(defvar sboo-long-line-length 80

  "How long a line is too long? In columns / characters.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-color-long-lines ()

  "Highlight over-long lines

See `sboo-long-line-length' (e.g ≥80 columns)."
  (interactive)

  (let ((REGEX
         (concat "^"
                 "[^\n]"
                 (format "\\{%d\\}" sboo-long-line-length)
                 "\\(.*\\)"
                 "$")))

    (font-lock-add-keywords nil `((,REGEX
                                   1
                                   font-lock-warning-face
                                   t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlight keywords:

(defcustom sboo-comment-keywords

  '( "TODO"
     "NOTE"
     "TEST"
     "FIXME"
     "BUG"
   )

  "Keywords (to highlight) within comments.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO (defvar sboo--cached--comment-keywords 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-regexp-of-strings (STRINGS)
 
  "A regexp that matches a string in `STRINGS'."

  (when (require 's nil :noerror)

    (s-wrap (s-join "\\|" STRINGS) "\\(" "\\)")))

;; ^ M-: (sboo-regexp-of-strings sboo-comment-keywords)
;;
;;     → (s-between "\\(" "\\)" (s-intercalate "\\|" '( "TODO" "NOTE" "TEST" "FIXME" "BUG" )))
;;
;;     → "\\(TEST\\|TODO\\|FIXME\\|BUG\\|NOTE\\)"
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-color-comment-keywords ()

  "Highlight universal keywords within comments.

See `sboo-comment-keywords' (e.g. NOTE and TODO)."
  (interactive)

  (let ((REGEX (concat "\\<" (sboo-regexp-of-strings sboo-comment-keywords)))
        )

  (font-lock-add-keywords nil `((,REGEX
                                 1
                                 font-lock-warning-face
                                 prepend)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `prog-mode' Hooks:

(defvar sboo-prog-mode-hooks

        '( #'sboo-color-long-lines
        ;; #'sboo-color-comment-keywords
         )

  "For `prog-mode-hook' (i.e. all program files).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;; 
;; 
;;

;;; Links
;;
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-prog-mode)