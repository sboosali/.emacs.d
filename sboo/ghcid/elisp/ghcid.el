;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'haskell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

;; (defgroup ghcid nil
;;   "GHCiD, a.k.a. the 'GHCi daemon' a.k.a. 'GHC interactive development'."
;;   :group 'haskell)

;; (defcustom ghcid-debug nil
;;   "Show debug output."
;;   :group 'ghcid
;;   :type '(set (const inputs) (const outputs) (const responses) (const command-line)))

;; (defcustom ghcid-repl-command-line nil
;;   "Command line to start GHCi, as a list: the executable and its arguments.
;; When nil, ghcid will guess the value depending on
;; `ghcid-project-root' contents.  This should usually be customized
;; as a file or directory variable.  Each element of the list is a
;; sexp which is evaluated to a string before being passed to the
;; shell."
;;   :group 'ghcid
;;   :type '(repeat sexp))

;; (defcustom ghcid-project-root nil
;;   "The project root, as a string or nil.
;; When nil, ghcid will guess the value by looking for a cabal file.
;; Customize as a file or directory variable."
;;   :group 'ghcid
;;   :type '(choice (const nil) string))

;; (put 'ghcid-project-root 'safe-local-variable #'stringp)

;; (defcustom ghcid-target nil
;;   "The target to demand from cabal repl, as a string or nil.
;; Customize as a file or directory variable.  Different targets
;; will be in different GHCi sessions."
;;   :group 'ghcid
;;   :type '(choice (const nil) string))

;; (put 'ghcid-target 'safe-local-variable #'stringp)

;; (defun ghcid-project-root ()
;;   "Get the root directory for the project.
;; If `ghcid-project-root' is set as a variable, return that,
;; otherwise look for a .cabal file, or use the current dir."
;;   (file-name-as-directory
;;    (or ghcid-project-root
;;        (set (make-local-variable 'ghcid-project-root)
;;             (file-name-directory (or (ghcid-cabal-find-file) (ghcid-buffer-file-name)))))))

;; (defun ghcid-repl-by-file (root files cmdline)
;;   "Return if ROOT / file exists for any file in FILES, return CMDLINE."
;;   (when (-any? (lambda (file) (file-exists-p (concat root file))) files) cmdline))

;; (defcustom ghcid-repl-command-line-methods-alist
;;   `((styx  . ,(lambda (root) (ghcid-repl-by-file root '("styx.yaml") '("styx" "repl" ghcid-target))))
;;     (nix   . ,(lambda (root) (ghcid-repl-by-file root '("shell.nix" "default.nix")
;;                                                       '("nix-shell" "--run" (concat "cabal repl " (or ghcid-target "") " --builddir=dist/ghcid")))))
;;     (stack . ,(lambda (root) (ghcid-repl-by-file root '("stack.yaml") '("stack" "repl" ghcid-target))))
;;     (mafia . ,(lambda (root) (ghcid-repl-by-file root '("mafia") '("mafia" "repl" ghcid-target))))
;;     (new-build . ,(lambda (root) (when (or (directory-files root nil ".+\\.cabal$") (file-exists-p "cabal.project"))
;;                                    '("cabal" "new-repl" ghcid-target "--builddir=dist/ghcid"))))
;;     (bare  . ,(lambda (_) '("cabal" "repl" ghcid-target "--builddir=dist/ghcid"))))
;; "GHCi launch command lines.
;; This is an alist from method name to a function taking the root
;; directory and returning either a command line or nil if the
;; method should not apply.  The first non-nil result will be used as
;; a command line.  Customize this if you do not want certain methods
;; to be used by default by ghcid.  If you want a specific
;; configuration for your project, customize
;; `ghcid-repl-command-line' directly, f as a directory-local
;; variable."
;;   :type '(alist :key-type symbol :value-type function))

;; (defvar ghcid-command-line "command line used to start GHCi")

;; (defun ghcid-repl-command-line ()
;;   "Return the command line for running GHCi.
;; If the custom variable `ghcid-repl-command-line' is non-nil, it
;; will be returned.  Otherwise, use
;; `ghcid-repl-command-line-methods-alist'."
;;   (or ghcid-repl-command-line
;;       (let ((root (ghcid-project-root)))
;;         (--first it (--map (funcall (cdr it) root)
;;                            ghcid-repl-command-line-methods-alist)))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Mode

;; (defvar ghcid-mode-map (make-sparse-keymap) "Ghcid minor mode's map.")

;; (defun ghcid-status ()
;;   "Return ghcid's status for the current source buffer."
;;   (let ((buf (ghcid-buffer-p)))
;;     (if (not buf) "stopped"
;;       (with-current-buffer buf
;;         (s-join ":"
;;            (-non-nil
;;             (list (format "%s" ghcid-state)
;;                   (when lcr-process-callback (format "busy(%s)" (1+ (length ghcid-queue)))))))))))

;; ;;;###autoload
;; (define-minor-mode ghcid-mode
;;   "Minor mode for Ghcid.
;; `ghcid-mode' takes one optional (prefix) argument.
;; Interactively with no prefix argument, it toggles ghcid.
;; A prefix argument enables ghcid if the argument is positive,
;; and disables it otherwise.
;; When called from Lisp, the `ghcid-mode' toggles ghcid if the
;; argument is `toggle', disables ghcid if the argument is a
;; non-positive integer, and enables ghcid otherwise (including
;; if the argument is omitted or nil or a positive integer).
;; \\{ghcid-mode-map}"
;;   :lighter (:eval (concat " Danté:" (ghcid-status)))
;;   :keymap ghcid-mode-map
;;   :group ghcid
;;   (if ghcid-mode
;;       (progn (flycheck-select-checker 'haskell-ghcid))
;;       (progn (flycheck-select-checker nil))))

;; (define-key ghcid-mode-map (kbd "C-c .") 'ghcid-type-at)
;; (define-key ghcid-mode-map (kbd "C-c ,") 'ghcid-info)
;; (define-key ghcid-mode-map (kbd "C-c /") 'attrap-attrap) ;; deprecated keybinding
;; (define-key ghcid-mode-map (kbd "C-c \"") 'ghcid-eval-block)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;

;; (defun ghcid-project-root ()
;;   "Get the root directory for the project.
;; If `ghcid-project-root' is set as a variable, return that,
;; otherwise look for a .cabal file, or use the current dir."
;;   (file-name-as-directory
;;    (or ghcid-project-root
;;        (set (make-local-variable 'ghcid-project-root)
;;             (file-name-directory (or (ghcid-cabal-find-file) (ghcid-buffer-file-name)))))))

;; (defun ghcid--cabal-new-build-command()
;;  ("cabal" "new-repl" ghcid-target "--builddir=dist/ghcid"))))

;; ;; e.g.
;; ;;
;; ;;    `(new-build . ,(lambda (root) (when (or (directory-files root nil ".+\\.cabal$") (file-exists-p "cabal.project"))
;; ;;                                    '("cabal" "new-repl" dante-target "--builddir=dist/dante"))))

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