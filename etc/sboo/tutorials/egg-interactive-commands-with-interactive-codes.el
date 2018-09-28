;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun egg-ask-name-and-age (x y)
  "Ask name and age."

  (interactive "sEnter you name: \nnEnter your age: ")

  (message "Name is: %s, Age is: %d" x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;