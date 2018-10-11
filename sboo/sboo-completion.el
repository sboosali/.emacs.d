;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; `dabbrev-expand'
;;
;; `M-/'
;;
;; > Expand the word before point as a dynamic abbrev,
;; > by searching for words starting with that abbreviation.
;;
;; Effect: Insertion
;;
;; Expansion = insert the nearest **single** suffix (or do nothing).
;;

;;; `dabbrev-completion'
;;
;; `C-M-/'
;;
;; > Complete the word before point as a dynamic abbrev.
;;
;; Effect: Popup
;;
;; Completion = a `*Completion*' buffer pops-up, with **all** suffices.
;; 
;; (for me, via `sboo-helm.el',
;; the completion buffer is named `*helm-mode-dabbrev-completion*').
;; 

;;; Links
;;
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion.html
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-completion)