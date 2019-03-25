;;; -*- lexical-binding: t -*-

;;==============================================;;
;;; Commentary:

;; Theme.
;; 
;; • 
;; • 
;;
;; 

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(require 'cl-lib)
;;(require 'pcase)
;;(require 'seq)

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

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

;;==============================================;;
(provide 'sboo-appearence)