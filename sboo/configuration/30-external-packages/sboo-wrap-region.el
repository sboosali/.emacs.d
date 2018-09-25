;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WRAP REGION: A FORMATTING PACKAGE
;;
;; Wrap Region is a minor mode for Emacs that wraps a region with punctuations. 
;; For "tagged" markup modes, such as HTML and XML, it wraps with tags.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(require 'use-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wrap-region
 :disabled
 :config
 (wrap-region-add-wrappers
  '(("{-" "-}" "#" (haskell-mode))))
 (wrap-region-global-mode t))
  ;; ^ select a region, then press "--";
  ;; this wraps that region in block comments;
  ;; only active for haskell buffers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; Usage
;;
;; [1] select a region
;;
;; [2] press a "wrap key", 
;;     e.g. one of the default wrappers:
;;       - "
;;       - (
;;       - {
;;       - [
;;       
;;     e.g. one of the custom wrappers (below):
;;       - $
;;       - #
;;       ....
;; 

;;;;;;;;;;
;; Configuration
;;
;; (wrap-region-mode t)
;; ;;  ^ activate globally
;;
;; (wrap-region-add-wrappers
;;  '(("$" "$")
;;    ("{-" "-}" "#")
;;    ("/" "/" nil ruby-mode)
;;    ("/* " " */" "#" (java-mode javascript-mode css-mode))
;;    ("`" "`" nil (markdown-mode ruby-mode))))
;;   ;;^ 
;;

;; `wrap-region-table':
;;
;; holds the default punctuation-wrappers.
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-wrap-region)