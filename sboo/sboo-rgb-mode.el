;;TODO provide-mode

;;TODO rainbow-mode

;;==============================================;;

;; font-lock "#FFFFEF" to (color ("#FFFFEF"))

;;----------------------------------------------;;

(defcustom sboo-sboo-rgb-hex-regexp

  (rx bos  eos
   )

  "Regular expression for .

e.g. matches « \"#AC09AB\" », colors Magenta."

  :type 'regexp

  :safe t
  :group 'sboo)


;; font-lock "#[0-9a-fA-f][0-9a-fA-f][0-9a-fA-f][0-9a-fA-f][0-9a-fA-f][0-9a-fA-f]"

;;==============================================;;
(provide 'sboo-rgb-mode)