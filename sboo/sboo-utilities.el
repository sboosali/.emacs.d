;; -*- lexical-binding: t; -*-

;;; Commentary:

;;----------------------------------------------;;
;; Utilities without Dependencies.
;;
;; Both General-Purpose and Special-Purpose.
;;
;; TODO clean up, del stuff.
;;----------------------------------------------;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtin packages:

(require 'cl)     ;; "CommonLisp"
(require 'pcase)  ;; "PatternCASE"

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Munging
;;----------------------------------------------;;

(defun sboo-tokenize (STRING) (let ((*separators* "[-/]+") (*omit-nulls* t)) (split-string STRING *separators* *omit-nulls*)))

;; ^
;; 
;;   >>> (sboo-tokenize "some-command")
;;   ("sboo" "command")
;;
;;   >>> (mapcar #'(lambda (s) (substring s 0 1)) '("sboo" "command"))
;;   ("s" "c")
;;
;;   >>> (apply #'string (mapcar #'string-to-char '("sboo" "command")))
;;   "sc"
;;
;; NOTE `sboo-tokenize' is inlined into `.yasnippet's
;; (keep it a one-liner, for easy transfer).
;;

;;----------------------------------------------;;

(defun sboo-abbreviate (STRING) (let ((*separators* "[-/]+") (*omit-nulls* t)) (apply #'string (mapcar #'string-to-char (split-string STRING *separators* *omit-nulls*)))))

;; ^
;; 
;;   >>> (sboo-abbreviate "some-command")
;;   "sc"
;;
;; NOTE `sboo-abbreviate' is inlined into `.yasnippet's
;; (keep it a one-liner, for easy transfer).
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Strings.html
;;

;;----------------------------------------------;;
;; Filesystem
;;----------------------------------------------;;
;;

(defun sboo-find-user-init-file ()
  (interactive)
  (find-file user-init-file))

;;----------------------------------------------;;

;; https://emacs.stackexchange.com/questions/7475/recursively-go-up-to-find-makefile-and-compile

(defun find-file-in-ancestor-directory (FILE)
  "Traveling backwards in the filesystem from the current file's directory, find FILE and open (or activate) it in another window. (TODO returning the buffer."
  (interactive)
  (let* ((directory (locate-dominating-file default-directory FILE))
         (filepath  (concat directory FILE)))
    (progn
      (if directory
       (find-file-other-window filepath)
       (message "[find-file-in-ancestor-directory] not found: `%s`" FILE)))))

;; (defun press-C-g ()
;;   (interactive)
;;   (setq unread-command-events (listify-key-sequence "\C-g")))

;;----------------------------------------------;;

;;----------------------------------------------;;
;;  

;; see http://ivanmalison.github.io/dotfiles/

(defmacro make-interactive-function (function)
  `(lambda (&rest args)
     (interactive)
     (apply ,function args)))

(defmacro measure-time-of (&rest body)
  "Measure the running time of the given code block, returning the result."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

(defun get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun get-last-message (&optional num)
  (or num (setq num 1))
  (if (= num 0)
      (current-message)
    (save-excursion
      (set-buffer "*Messages*")
      (save-excursion
    (forward-line (- 1 num))
    (backward-char)
    (let ((end (point)))
      (forward-line 0)
      (buffer-substring-no-properties (point) end))))))

(defun random-choice-from (choices)
  (nth (random (length choices)) choices))

(defun eval-region-or-last-sexp ()
  (interactive)
  (if (region-active-p) (call-interactively 'eval-region)
    (call-interactively 'eval-last-sexp)))

;;----------------------------------------------;;

(defun sboo-kill-file-buffers-matching-file-extension (SUFFIX)
  ""
  (interactive)
  )

;;----------------------------------------------;;

(defun sboo-message-warning (input)

  (interactive)
  "(by @lawlist)."
  (message ;;;"%s"
    (propertize input 'face 'font-lock-warning-face)))

;; ^

;-;-;-;-;-;-;-;-;-;-;-;-;

;; See:
;;     - https://www.gnu.org/s/emacs/manual/html_node/elisp/Displaying-Messages.html
;;     - https://stackoverflow.com/questions/2742435/in-emacs-how-do-i-display-a-message-in-the-minibuffer-with-font-face-properties
;;

;;----------------------------------------------;;

(defun sboo-copy-buffer-filepath-to-clipboard ()
  "Put the current file name on the clipboard.
   
   From `https://stackoverflow.com/a/2417617/1337806'. 
   "
  (interactive)

  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;;----------------------------------------------;;

(defun sboo-restore-default-shell-command-switch ()
   "Restores the default `shell-command-switch'. 

   i.e.:

   * `-i', \"run terminal interactively\";
   * `-c', \"run the following command\".

   "
   (interactive)

   (progn
     (setq
      shell-command-switch "-ic")))

;;----------------------------------------------;;
;; Hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;----------------------------------------------;;

(defvar sboo-hook-regex "-\\(hook\\|functions\\)$"

  "By convention, a variable is a hook if it ends with « -hook » or « -functions ».")

;;----------------------------------------------;;

(defun sboo-hook-p (VARIABLE)
  "Whether the symbol `VARIABLE' is a hook.

Examples:

    M-: (sboo-hook-p 'find-file-hook)
    t

    M-: (sboo-hook-p 'tooltip-functions)
    t

    M-: (sboo-hook-p 'not-a-hook-variable)
    nil
"

  (let ((STRING (symbol-name VARIABLE)))
    (integerp (string-match sboo-hook-regex STRING))))

;; ^

;;----------------------------------------------;;

(defun sboo-get-hook-variables ()
  "Return a list of all hooks.

i.e. Global variables matching `sboo-hook-regex'."

  (let ((HOOKS '()))

    (cl-flet ((ADD-HOOK (s)
                        (if (sboo-hook-p s) (push s HOOKS))))

      (progn
        (mapatoms #'ADD-HOOK obarray)
        HOOKS))))

;; ^ NOTE `add-to-list' doesn't work with local variables?

;; ^ Examples:
;;
;;     M-: (length (sboo-get-hook-variables))
;;     1011
;;

;;----------------------------------------------;;

(defun sboo-read-hook ()

  "Read a hook variable (which exists), with completion, returning the symbol.

See `sboo-hook-regex'."

  (intern-soft
   (completing-read "Hook: " (sboo-get-hook-variables) nil t)))

;;----------------------------------------------;;
;; Utilities: Copying --------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-deep-copy (x)

  "Copy datatype X deeply (i.e. recursively).

Inputs:

• X — is a datatype.

      X can be primitive datatype: `booleanp', `symbolp', `stringp', `integerp', `floatp', `functionp'.

      X can be collection datatype: `consp', `listp', `vectorp', `arrayp', `bool-vector-p', `hash-table-p'.

      X can't be an 'implementation' type: `subrp', `byte-code-function-p', etc.

      X can't be an editor type: `bufferp', `case-table-p', `char-table-p', `condition-variable-p', `custom-variable-p', `fontp', `frame-configuration-p', `framep', `keymapp', `markerp', `mutexp', `overlayp', `processp', `recordp', `sequencep', `syntax-table-p', `threadp', `window-configuration-p', `windowp' etc.

Output:

• a new value Y that should `equal' (but shouldn't `eq') X.

NOTE:

• loops infinitely if X is a recurisve value."

  (pcase x

    ((pred symbolp)       x)
    ((pred integerp)      x)
    ((pred floatp)        x)
    ((pred functionp)     x)
    ((pred stringp)       (mapcar #'sboo-deep-copy x))

    ((pred consp)         (sboo-deep-copy-cons x))

    ((pred listp)         (mapcar #'sboo-deep-copy x))
    ((pred vectorp)       (mapcar #'sboo-deep-copy x)) ;;TODO;; we must recreate the given collection. « mapcar » is always a list.
    ((pred arrayp)        (mapcar #'sboo-deep-copy x)) ;;TODO;; we must recreate the given collection. « mapcar » is always a list.
    ((pred bool-vector-p) (mapcar #'sboo-deep-copy x)) ;;TODO;; we must recreate the given collection. « mapcar » is always a list.
    ((pred hash-table-p)  (mapcar #'sboo-deep-copy x)) ;;TODO;; we must recreate the given collection. « mapcar » is always a list.

    (_                    x)))

;; ^ Notes
;;
;; • 
;;
;; • M-: (listp '(x . 1))
;;       t
;;

;;----------------------------------------------;;

(cl-defun sboo-deep-copy-cons (cons)

  "Copy CONS deeply (i.e. recursively).

Inputs:

• CONS — is a `cons' cell.

Output:

• an `cons' with the same `car' and `cdr' as CONS."

  (pcase cons

    (`(,x . ,y)

     (cons (sboo-deep-copy x) (sboo-deep-copy y)))

    (_ cons)))

;;----------------------------------------------;;

(cl-defun sboo-move-to-head-of-alist (alist &key key)

  "Move KEY and its value to the `car' of ALIST.

Inputs:

• KEY — is one of: `symbolp', `stringp', or `numberp'.

• ALIST — is an `alist' (TODO or a symbol variable).
          its key-type is equal to the `type-of' KEY.

Output:

• an `alist'.

• the output has the same keys and values as the input (i.e. as ALIST).

• [TODO ALIST is mutated too.]

Examples:

• M-: (sboo-move-to-head-of-alist '((x . 1) (y . 2) (z . 3)) :key 'y)
    ⇒ '((y . 2) (x . 1) (z . 3))
  
Laws:

• is idempotent."

  (let ((ALIST (sboo-deep-copy alist))
        (KEY   (sboo-deep-copy key))
        )

    (let ((DEFAULT       :sboo-not-found)
          (REMOVE?       t)
          (TEST          (cond
                          ((stringp KEY) #'string-equal)
                          (t             #'eql)))
          )

      (let* ((VALUE          (alist-get KEY ALIST DEFAULT REMOVE? TEST))
             (ATTR           (cons KEY VALUE))
             (WAS-KEY-FOUND? (not (eql VALUE DEFAULT)))
             )

        (if WAS-KEY-FOUND?  

            (cons ATTR
                  (if REMOVE?
                      (cl-delete KEY ALIST :key #'car :test TEST)
                    ALIST))

          ALIST)))))

;;----------------------------------------------;;
;; -------------------------------------;;
;;----------------------------------------------;;

;TODO URL `http://ergoemacs.org/emacs/elisp_change_brackets.html'.

;;----------------------------------------------;;
;; -------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;;; Notes: -------------------------------------;;
;;----------------------------------------------;;

;;; DOCS `string-match'
;;
;; Examples:
;;
;;     M-: (string-match "-\\(hook\\|functions\\)$" "find-file-hook")
;;     9
;;
;;     M-: (string-match "-\\(hook\\|functions\\)$" "tooltip-functions")
;;     7
;;
;;     M-: (string-match "-\\(hook\\|functions\\)$" "some-non-hook-variable")
;;     nil
;;
;;     M-: (integerp (string-match "-\\(hook\\|functions\\)$" "find-file-hook"))
;;     t
;;

;;----------------------------------------------;;
(provide 'sboo-utilities)