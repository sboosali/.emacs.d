;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `company' Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `company-backends' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode-specific `company-backends': Haskell



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode-specific `company-backends': ELisp



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode-specific `company-backends': bash



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode-specific `company-backends': Python

(defun sboo-company-python ()
  
  (add-to-list (make-local-variable 'company-backends)
               #'company-anaconda))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode-specific `company-backends': JavaScript

(defun sboo-company-javascript ()

  (tern-mode t)

  (add-to-list (make-local-variable 'company-backends)
               #'company-tern))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `company-complete-number' Utilities ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `company-complete-number' :
;;
;; (company-complete-number N)
;; Insert the Nth candidate visible in the tooltip.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-company-complete-1 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-company-complete-2 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-company-complete-3 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-company-complete-4 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-company-complete-5 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-company-complete-6 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-company-complete-7 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-company-complete-8 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-company-complete-9 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-company-complete-10 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-company-complete-common-or-previous-cycle ()
  "Backwards `company-complete-common-or-cycle'."
  (interactive)
  (company-complete-common-or-cycle -1))

;; ^ `company-complete-common-or-cycle' :
;; 
;; > "Insert the common part of all candidates, or select the next one."
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; `'

;;; `'

;;; `company-backends'
;;
;; `company-backends' default value is:
;;
;; (`company-bbdb'
;;  `company-nxml'
;;  `company-css'
;;  `company-eclim'
;;  `company-semantic'
;;  `company-clang'
;;  `company-xcode'
;;  `company-cmake'
;;  `company-capf'
;;  `company-files'
;;  (`company-dabbrev-code'
;;   `company-gtags'
;;   `company-etags'
;;   `company-keywords')
;;  `company-oddmuse'
;;  `company-dabbrev')
;;
;; `company-backends' type is a `list', where each item is either:
;;
;; - a backend; has type `function'.
;; - a backend group; has type `list' of `function'.
;; 

;;; CompanyBackend
;; 
;; Each CompanyBackend has this signature:
;; 
;; (defun company-<name>-backend (COMMAND &optional _ &rest _) ...)
;; 
;; where:
;; 
;; - `COMMAND' is a `symbol' (see below)
;; - 
;; - 
;; 

;;; CompanyCommand
;; 
;; A CompanyBackend takes a CompanyCommand.
;; 
;; CompanyCommand is a `symbol', one of:
;; 

;;; Links
;;
;; - 
;; - 
;; - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-company)