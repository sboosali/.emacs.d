(provide 'my-template)
;(require 'template)

;(template-initialize)

;(add-to-list 'template-find-file-commands 'ido-exit-minibuffer)


;http://emacs-template.sourceforge.net/

;$ mkdir ~/.templates/
;$ touch ~/.templates/TEMPLATE.hs.tpl
;$ ls ~/.templates/*.tpl

;M-x template-new-file
;M-x template-expand-template

;some standard embedded tags (note the parentheses):
;; (>>>POINT<<<)
;; (>>>FILE_SANS<<<)
;; (>>>DATE<<<)
;; (>>>USER_NAME<<<)

; e.g. defines the tag (>>>GUIID<<<)
;(add-to-list 'template-expansion-alist
; '("GUIID" (insert (substring (shell-command-to-string "uuidgen") 0 -1))))

