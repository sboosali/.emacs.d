

;; FONTS

(1) font-family-list:

in Emacs:

  M-: (print (font-family-list))

You can see the result in “*Messages*” buffer, 

  view-echo-area-messages
【Ctrl+h e】




(2) fc-list:

 on Linux, in terminal:

  $ fc-list





(cond


 ((string-equal system-type "windows-nt") 
  ;; ^ Microsoft Windows
  (...))

 ((string-equal system-type "darwin")
  ;; ^ macOS
  (...))

 ((string-equal system-type "gnu/linux")
  ;; ^ linux
  (...))

)






;; M-x eshell
;; 
;; try:
;; - man ls
;;



;; M-x describe-char
;; then, the line in “display:” shows the font used for the character under cursor.

;; M-x text-scale-adjust
;; 
;; then press:
-【+】to increase
-【-】 to decrease
-【0】to set it back to default size.
- any other key to exit.




(setq sboo-frame-alist
           '( (tool-bar-lines . 0)
              (width . 106) ; chars
              (height . 60) ; lines
              (background-color . "honeydew")
              (left . 50)
              (top . 50)))
  # ^ M-x list-colors-display
  # shows a list of color-names that emacs supports.

(add-to-list 'default-frame-alist
  '(font . "DejaVu Sans Mono-10"))
# ^ means:
# - the font-face is: "DejaVu Sans Mono"
# - the font-size is: 10






(if (display-graphic-p)
    (progn


      (setq initial-frame-alist sboo-frame-alist)
  # ^ initial-frame-alist is a variable that holds an association-list of settings for: 
  # the first window emacs starts with.


      (setq default-frame-alist sboo-frame-alist)))
  # ^ default-frame-alist is a variable that holds an association-list of settings for: 
  # any new window.






