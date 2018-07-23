;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'haskell-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(progn
 (setq compilation-warning-type    0)
 ;; ^ 
 (setq compilation-info-type       1)
 ;; ^ 
 (setq compilation-error-type      2)
 ;; ^ 
)
;; ^ constants that are aliased for readability.

(progn
 (setq ghcid-filename   10)
 ;; ^ 
 (setq ghcid-line       20)
 ;; ^ 
 (setq ghcid-column     30)
 ;; ^ 
)
;; ^ these numbers are arbitrary.
;; they identify different regular expression groups.

(progn
 (setq ghcid-type       compilation-error-type)
 ;; ^ 
 (setq ghcid-hyperlink  nil)
 ;; ^ `nil` means: match the whole line
 ;(setq ghcid-highlight  )
 ;; ^ 
)
;; ^ 
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the Ghcid Regex: Definition

;; NOTE:
;; for this regex, `ghcid` needs this option:
;;
;;     --ghci-options -ferror-spans
;;

(progn

 (setq ghcid-error-regular-expression 
   (rx 
 
    line-start
 ;;TODO?   line-start 
 
    (group-n 10 ;;TODO ghcid-filename
             (one-or-more (not (any ":\n")))
 	    ".hs") ;;TODO other extensions, like: .lhs, .hsc, 
    ":"
    (group-n 20  ;;TODO ghcid-line
 	    (one-or-more digit))
 
    ;;NOTE -ferror-spans only
    ;; which cabal new-repl lacks
    ;; (group-n 21
    ;;          (zero-or-one "-" (one-or-more digit))) 
 
    ":"
    (group-n 30  ;;TODO ghcid-column
 	    (one-or-more digit))
 
    ;;NOTE -ferror-spans only
    ;; which cabal new-repl lacks
    ;; (group-n 31
    ;;          (zero-or-one "-" (one-or-more digit))) 
 
    ": " 
    (or (group-n 42 "error")    ;;TODO 
        (group-n 41 "warning"))  ;;TODO 
    ":"
    (zero-or-more (not (any "\n")))
 
    line-end
 
    ;; (zero-or-more (not (any "\n")))
    ;; "error" 
    ;; (zero-or-more (not (any "\n")))
 
 ;;TODO?    line-end
 )) 
 
 ;;
 (setq ghcid-warning-regular-expression
   (rx 
    line-start
    (group-n 10 ;;TODO ghcid-filename
             (one-or-more (not (any ":\n")))
 	    (or ".hs" ".lhs" ".hsc"))
    ":"
    (group-n 20  ;;TODO ghcid-line
 	    (one-or-more digit))
    ":"
    (group-n 30  ;;TODO ghcid-column
 	    (one-or-more digit))
    ": warning:"
    (zero-or-more (not (any "\n")))
    line-end)) 
 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the Ghcid Regex: Configuration

(progn

 (add-to-list 'compilation-error-regexp-alist       
  'ghcid-error)
 
 (add-to-list 'compilation-error-regexp-alist       
  'ghcid-warning)
 
 (add-to-list 'compilation-error-regexp-alist-alist
  (list 'ghcid-error
        ghcid-error-regular-expression
        ghcid-filename
        ghcid-line
        ghcid-column
        compilation-error-type
        ghcid-hyperlink
  )
 ) ;; (list 1 'compilation-error-face)))
 
 (add-to-list 'compilation-error-regexp-alist-alist
  (list 'ghcid-warning
        ghcid-warning-regular-expression
        ghcid-filename
        ghcid-line
        ghcid-column
        compilation-warning-type
        ghcid-hyperlink))
 
 ;; defvar
 ;; (setq ghcid-file-regular-expression 
 ;;   (rx 
 ;;  ;;  string-start 
 ;;    (zero-or-more anything)
 ;;    "ghcid.txt"
 ;;    (zero-or-more anything)
 ;; ;;   string-end
 ;;   )) 
 
 (setq ghcid-file-regular-expression ".*ghcid\\.txt\\'")
 
 (add-to-list 'auto-mode-alist
  (cons ghcid-file-regular-expression 'ghcid-mode))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utility Functions

(progn
 (setq ghcid-file-name-default ".ghcid")
 (setq ghcid-file-regex        "*.ghcid") ;;TODO
 ;; ^ 
 ;; 
)

(defun find-dot-ghcid ()
 "open/activate the nearest `*.ghcid`, if it exists. works within multi-package projects too." 
 (interactive)
 (find-file-in-ancestor-directory ghcid-file-name-default)
 (beginning-of-buffer)
 (revert-buffer t t)
 (setq unread-command-events (listify-key-sequence "\C-m"))
 ;; (setq unread-command-events (kbd "RET"))
)
;; ^ 
;; TODO .hs extensions only?"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings

(progn

 (global-set-key (kbd "<kp-home>")	'dante-type-at)		; C-c .
 (global-set-key (kbd "<kp-end>")	'dante-info)		; C-c ,
 
 (global-set-key (kbd "<kp-delete>")     	'find-dot-ghcid)
 
 ;;(global-set-key (kbd "<kp-space>")     	'xref-find-definitions)	; M-.
 
 ;; (global-set-key (kbd "<kp-decimal>")     	'xref-find-definitions)	; M-.
 ;; (global-set-key (kbd "<kp-up>")	'xref-find-definitions)	; M-.
 ;; (global-set-key (kbd "<kp-down>")	'xref-find-references)	; M-?
 ;; (global-set-key (kbd "<kp-prior>")     	'xref-find-definitions)	; M-.
 ;; (global-set-key (kbd "<kp-next>") 	'xref-find-references)	; M-?
 
 ;; (global-set-key (kbd "<kp-KEY>")	'dante-eval-block)	; C-c ”
 ;; (global-set-key (kbd "<kp-KEY>") 'dante-auto-fix) 		; C-c /
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; the Minor Mode

(defun ghcid-mode ()
  (interactive)
  
  (read-only-mode 1)
  
  (progn
   (setq auto-revert-interval 1)
   ;; ^ `1` means: revert every second.
   (setq auto-revert-verbose nil)
   ;; ^ suppress the message that the buffer has been reverted.
  )

  (auto-revert-mode 1)

  (compilation-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;NOTES

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `-ferror-spans`
;;
;; cabal new-repl lacks (?) `-ferror-spans` 
;; > 10 (cons 20 21) (cons 30 31)
;; (setq ghcid-line-range       (20 . 21))
;; (setq ghcid-column-range     (30 . 31))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `alist`s
;;
;; see https://www.emacswiki.org/emacs/AutoModeAlist
;; The auto-mode-alist variable is an AssociationList that associates MajorModes with a pattern to match a buffer filename when it is first opened
;; the string matched is the full pathname
;; { \\' } means "match the end of the string" ?? 
;; Backslashes must be entered as { \\. } ??  

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the `regex` generated by `rx`
;;
;; "^\\(?10:[^:]+\\.hs\\):\\(?20:[[:digit:]]+\\):\\(?30:[[:digit:]]+\\): error:$"
;; ^\(?10:[^:]+\.hs\):\(?20:[[:digit:]]+\):\(?30:[[:digit:]]+\): error:$
;; ^[^:]+\.hs:[[:digit:]]+:[[:digit:]]+: error:$

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `revert-buffer`
;;
;; (revert-buffer &optional IGNORE-AUTO NOCONFIRM PRESERVE-MODES)
;;
;; http://ergoemacs.org/emacs/emacs_key_notation_return_vs_RET.html
;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Regexes
;;
;; (rx bol
;;   (zero-or-more blank)
;;   (one-or-more digit)
;;   ":")
;; 
;; ->
;; 
;; "^[[:blank:]]*[[:digit:]]+:"
;;
;; (rx 
;;   line-start 
;;   "C:\\" 
;;   (one-or-more (or alphanumeric (or blank "-" "_" ) (or "\\" "/"))) 
;;   ".hs"
;;   ":"
;;   (one-or-more digit))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compilation-Mode
;;
;; (setq compilation-error-regexp-alist-alist ) 
;;
;; see compilation-error-regexp-alist 
;; (REGEXP FILE [LINE COLUMN TYPE HYPERLINK HIGHLIGHT...]).
;; If REGEXP matches, the FILE’th subexpression
;; gives the file name, and the LINE’th subexpression gives the line
;; number, the COLUMN’th subexpression gives the column number on
;; that line.
;; LINE can also be of the form (LINE . END-LINE) meaning a range
;; of lines.  COLUMN can also be of the form (COLUMN . END-COLUMN)
;; meaning a range of columns starting on LINE and ending on
;; END-LINE, if that matched.
;; What matched the HYPERLINK’th subexpression has ‘mouse-face’ and
;; ‘compilation-message-face’ applied.  If this is nil, the text
;; matched by the whole REGEXP becomes the hyperlink.
;; TYPE is 2 or nil for a real error or 1 for warning or 0 for info.
;; TYPE can also be of the form (WARNING . INFO).  In that case this
;; will be equivalent to 1 if the WARNING’th subexpression matched
;; or else equivalent to 0 if the INFO’th subexpression matched.
;; See ‘compilation-error-face’, ‘compilation-warning-face’,
;; ‘compilation-info-face’ and ‘compilation-skip-threshold’.
;;
;; lookup the value of a key in an association list 
;; (cdr (assoc 'KEY ALIST))
;; 
;; (assoc 'java compilation-error-regexp-alist-alist) 
;; (java "^\\(?:[ 	]+at \\|==[0-9]+== +\\(?:at\\|b\\(y\\)\\)\\).+(\\([^()\n]+\\):\\([0-9]+\\))$" 2 3 nil (1))
;;
;; example: 
;; 
;; (add-to-list 'compilation-error-regexp-alist-alist
;;      '(my-message-error
;;        "^\\(ERRR\\|CRIT\\|ALRT\\|EMRG\\) .*\\[\\(\\([^ \n]+\\):\\([0-9]+\\)\\)\\]$"
;;        3 4 nil nil 2
;;        (1 compilation-error-face)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `global-set-key` with keypad (`kp`) keys
;;
;; (global-set-key (kbd "<kp-KEY>") 'COMMAND)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'ghcid)