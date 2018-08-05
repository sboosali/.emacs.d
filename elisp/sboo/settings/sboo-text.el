;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TextProperties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; Text in a buffer can have “text properties”.
;;
;; Text properties are used to:
;; - color text,
;; - to add keybinding to that text (for example, pressing Enter on a link in HTML opens the link, instead of inserting return.),
;; - <etc>.

;; e.g. make the selection red:
;;
;;     (defun xah-make-word-red (begin end)
;;       "Make current region colored red, using text properties"
;;       (interactive "r")
;;       (put-text-property begin end 'font-lock-face
;;         '(:foreground "red")))
;;
;; See:
;;     - http://ergoemacs.org/emacs/elisp_text_properties.html
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-)