;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `company' Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `company-backends' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Default (/ Global) `company-backends'

(setq sboo-company-backends
      
      '(
        (company-files          ; files & directory
         company-keywords       ; keywords
         company-capf
         company-yasnippet)
        
        (company-abbrev
         company-dabbrev))
      )

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