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

(require 'cl-lib)     ;; "CommonLisp"
(require 'pcase)  ;; "PatternCASE"
(require 'subr-x) ;; "SUBRoutines-eXtras"

;;----------------------------------------------;;
;; Macros --------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;

(eval-when-compile

  (defmacro sboo-defpure (name arguments docstring &rest expressions)

    "expression.

Inputs:

• NAME        — the function name (a symbol, unquoted).
• ARGUMENTS   — the function's argument-list (a list of symbols, unquoted).
• DOCSTRING   — the function's documentation (a string).
• EXPRESSIONS — the function's body (a list of S-expressions, unquoted).

Output:

• a `defun' declaration.

Example:

• M-: (pp-macroexpand-expression (sboo-defpure sboo-increment (x) \"Increment X.\" (+ 1 x)))

    ⇒ (defun sboo-increment (x)
        \"Increment X.\"
        (declare (pure t) (side-effect-free t))
        (+ 1 x))

Links:

• Info node `(elisp) Standard Properties'.

Related:

• `defun'"

    (declare (indent 2) (doc-string 3))

    (let* ()

      `(defun ,name ,arguments
         ,docstring
         (declare (pure t) (side-effect-free t))
         (progn ,expressions))

      )))

;; e.g.: (pp-macroexpand-expression (sboo-defpure sboo-increment (x) "" (+ 1 x)))
;;
;; (defun sboo-increment (x)
;;   (declare (pure t) (side-effect-free t))
;;   (+ 1 x))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Properties.html

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Utilities: Debugging ------------------------;;
;;----------------------------------------------;;

;;TODO sboo-goto-char-from-error-line

;; ^ e.g. parse «  eval-buffer(#<buffer  *load*-708054> nil "/home/sboo/.emacs.d/sboo/sboo-init.el" nil t)  ; Reading at buffer position 7897 » into:
;;   '(file "/home/sboo/.emacs.d/sboo/sboo-init.el" char 7897)

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


(defun sboo-read-file (path)

      "Read PATH.

Inputs:

• PATH — a `stringp'. the filepath should:

    • exists, as a file (`file-exists-p').
    • is readable (`file-readable-p').

Output:

• a `stringp'. 

Links:

• URL `http://ergoemacs.org/emacs/elisp_read_file_content.html'"

  (if (and (file-exists-p path) (file-readable-p path))

    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))

  (progn
    (message "« sboo-read-file %S »: PATH doesn't exile or isn't readable." path)
    nil)))

;;----------------------------------------------;;

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
;; Hooks ---------------------------------------;;
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
;; Utilities: yasnippet ------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-yasnippet-condition (&key key (indentation 4))

  "A predicate for a `yasnippet' « condition: » property.

Inputs:

• KEY — the `yasnippet' key (to be expanded).

Output:

• a boolean — Compares `current-column' against the `string-width' of KEY.

Example (yasnippet header):

    # -*- mode: snippet -*-
    # key         : defcustom
    # condition   : (let ((KEY \"defcustom\")) (condition-case nil (sboo-yasnippet-condition :key KEY :indentation 0) (void-function (= (current-column) (string-width KEY)))))
    # --

Notes (implementation):

• `string-width' — the number of columns the string is displayed across (in the current buffer). TAB chars display across `tab-width' columns.

Links:

• Info node `(elisp) Handling Errors'."

  (let* ((COLUMN      (current-column))
         (LENGTH      (string-width key))
         )

    (or (= LENGTH COLUMN)

        (when (and indentation (numberp indentation) (> indentation 0))
          (and (>= (- COLUMN LENGTH) 0)
               (<= (- COLUMN LENGTH) indentation))))))

;;==============================================;;

(cl-defun sboo-serialize (&key data file)

  "Write DATA to FILE."

  (interactive (list
                (symbol-value (intern-soft (completing-read "Serialize variable: " obarray nil t)))
                (read-file-name "Write to file: ")
                ))

  (with-temp-file file

    (prin1 data (current-buffer))))

;;----------------------------------------------;;

(cl-defun sboo-deserialize (&key file symbol)

  "Read FILE into SYMBOL."

  (interactive (list
                (read-file-name "Read from file: ")
                (read-string "Deserialize into variable: ")
                ))

  (with-temp-buffer

    (insert-file-contents file)
    (goto-char (point-min))
    (set symbol (read (current-buffer)))))

;;==============================================;;

(defun xah-css-normalize-number-scale (@val @range-max)
  "Scale *val from range [0, *range-max] to [0, 1]
The arguments can be int or float.
Return value is float.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2016-07-19"
  (/ (float @val) (float @range-max)))

;;----------------------------------------------;;

(defun xah-css-convert-color-hex-to-vec (@rrggbb)
  "Convert color *rrggbb from “\"rrggbb\"” string to a elisp vector [r g b], where the values are from 0 to 1.
Example:
 (xah-css-convert-color-hex-to-vec \"00ffcc\") ⇒ [0.0 1.0 0.8]

Note: The input string must NOT start with “#”.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2016-07-19"
  (vector
   (xah-css-normalize-number-scale (string-to-number (substring @rrggbb 0 2) 16) 255)
   (xah-css-normalize-number-scale (string-to-number (substring @rrggbb 2 4) 16) 255)
   (xah-css-normalize-number-scale (string-to-number (substring @rrggbb 4) 16) 255)))

;;----------------------------------------------;;

(defun xah-css-hex-color-to-hsl ()
  "Convert color spec under cursor from “#rrggbb” to CSS HSL format.
 e.g. #ffefd5 ⇒ hsl(37,100%,91%)
URL `http://ergoemacs.org/emacs/elisp_convert_rgb_hsl_color.html'
Version 2016-07-19"
  (interactive)

  (require 'color)

  (let* (
         ($bds (bounds-of-thing-at-point 'word))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($currentWord (buffer-substring-no-properties $p1 $p2)))
    (if (string-match "[a-fA-F0-9]\\{6\\}" $currentWord)
        (progn
          (delete-region $p1 $p2 )
          (when (equal (char-before) 35) ; 35 is #
            (delete-char -1))
          (insert (xah-css-hex-to-hsl-color $currentWord )))
      (progn
        (user-error "The current word 「%s」 is not of the form #rrggbb." $currentWord)))))

;;----------------------------------------------;;

(defun sboo-css-hex-to-hsl-color (hex-str &optional was-called-interactively)

  "Convert HEX-STR color to CSS HSL format.

Output:

• a string. 

Examples: 

• M-: (sboo-css-hex-to-hsl-color \"ffefd5\")
    ⇒ \"hsl(37,100%,91%)\"

Note: The input string must NOT start with “#”.

Links:

• URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'"

  (interactive (list (read-string "Hex String (case-insensitive) (e.g. « AC00AB »): ") ;TODO force length=6 (or 3, or 1) and case-insensitive hexadecimal.
                     t
                     ))

  (require 'color)

  (let* (($colorVec (xah-css-convert-color-hex-to-vec hex-str))

         ($R        (elt $colorVec 0))
         ($G        (elt $colorVec 1))
         ($B        (elt $colorVec 2))

         ($hsl      (color-rgb-to-hsl $R $G $B))

         ($H        (elt $hsl 0))
         ($S        (elt $hsl 1))
         ($L        (elt $hsl 2))

         ($h        (* $H 360))
         ($s        (* $S 100))
         ($l        (* $L 100))

         (hsl-string (format "hsl(%d,%d%%,%d%%)" $h $s $l))
         )

    (when was-called-interactively
      (message "%s" hsl-string))

    hsl-string))

;;==============================================;;

(cl-defun sboo-move-to-head-of-alist! (alist-var &key key)

  "`sboo-move-to-head-of-alist' with mutation.

Inputs:

• KEY — is one of: `symbolp', `stringp', or `numberp'.

• ALIST-VAR — is a symbol, representing an `alist' variable.
          its key-type is equal to the `type-of' KEY.

Output:

• ALIST-VAR.

Examples:

• M-: (setq sboo-xyz '((x . 1) (y . 2) (z . 3) (y . 4)))
• M-: (sboo-move-to-head-of-alist! 'sboo-xyz :key 'y)
    ⇒ '((y . 2) (x . 1) (z . 3))
• M-: sboo-xyz
    ⇒ '((y . 2) (x . 1) (z . 3))

• M-: (setq sboo-abc '((\"a1\" . 1) (\"b2\" . 2) (\"c3\" . 3)))
• M-: (sboo-move-to-head-of-alist! 'sboo-abc :key \"b2\")
    ⇒ '((\"b2\" . 2) (\"a1\" . 1) (\"c3\" . 3))
• M-: sboo-abc
    ⇒ '((\"b2\" . 2) (\"a1\" . 1) (\"c3\" . 3))

Laws:

• is idempotent."

  (let ((ASSERTION (boundp alist-var))
        )

    (if ASSERTION

        (let* ((alist          (symbol-value alist-var))

               (DEFAULT        :sboo-not-found)
               (TEST           #'equal)

               (VALUE          (alist-get key alist DEFAULT nil TEST))
               (WAS-KEY-FOUND? (not (eql VALUE DEFAULT)))
               )

          (if WAS-KEY-FOUND?

              (let* ((PREDICATE (lambda (KV)
                                  (when (consp KV)
                                    (let ((K (car KV)))
                                      (equal key K)))))
                     (ATTR      (cons key VALUE))
                     )
                (set alist-var
                     (cons ATTR (seq-remove PREDICATE alist))))

            alist))

      (format-message "[sboo-move-to-head-of-alist!] assertion failed in « sboo-move-to-head-of-alist! ALIST-VAR :key KEY »: ALIST-VAR must be a `boundp' symbol; the ALIST-VAR given was « %S »."
                      alist-var))))

;; ^ Notes
;;
;; `alist-get' doesn't invoke `symbol-value' (c.f. `add-to-list'):
;;
;; M-: (alist-get 'y 'sboo-xyz)
;; Wrong argument type, `listp': sboo-xyz »
;;
;; `seq-remove' removes all:
;;
;; M-: (seq-remove  (lambda (KV) (when (consp KV) (let ((K (car KV))) (equal 'y K))))  '((x . 1) (y . 2) (z . 3) (y . 4)))
;;  ⇒ '((x . 1) (z . 3))
;;
;; `seq-remove' doesn't mutate:
;;
;; M-: (progn  (setq sboo-xyz '((x . 1) (y . 2) (z . 3) (y . 4)))  (seq-remove  (lambda (KV) (when (consp KV) (let ((K (car KV))) (equal 'y K))))  (symbol-value 'sboo-xyz))  (symbol-value 'sboo-xyz))
;;  ⇒ '((x . 1) (y . 2) (z . 3) (y . 4))
;;
;; `add-to-list' can postpend (by default, it prepends):
;;
;; M-: (progn  (setq sboo-xyz '((x . 1) (y . 2) (z . 3) (y . 4)))  (add-to-list 'sboo-xyz '(a . 5) t)  (symbol-value 'sboo-xyz))
;;  ⇒ '((x . 1) (y . 2) (z . 3) (y . 4) (a . 5))
;;
;; 
;;
;; 
;;

;;==============================================;;

(defun sboo-byte-compiled-p (func)

  "Whether function FUNC has been `byte-compile'd.

Inputs:

• FUNC — a `symbolp' or `functionp'. Otherwise (for objects of other types), output nil.

Links:

• URL `https://nullprogram.com/blog/2010/07/01'"

  (cond
   ((symbolp   func) (byte-compiled-p (symbol-function func)))
   ((functionp func) (not (sequencep func)))
   (t nil)))

;;----------------------------------------------;;

(defun sboo-require (feature)

  "`require' feature FEATURE.

Interactive `use-package'

Inputs:

FEATURE — a symbol.
          When invoked interactively, FEATURE comes from `sboo-read-feature'.
          By default, a personal feature (i.e. named `sboo-*')."

  (interactive (list
                (sboo-read-feature :require-match nil)))

  (require feature nil :no-error))

;;----------------------------------------------;;

(defun keyword-to-symbol (keyword)

  "Convert `keywordP' KEYWORD to a `symbolp'.


Examples:

• M-: (keyword-to-symbol :foo)
    ⇒ 'foo

Links:

• URL `https://emacsredux.com/blog/2019/01/10/convert-a-keyword-to-a-symbol/'"

  (intern (substring (symbol-name keyword) 1)))

;;----------------------------------------------;;

;; TODO
;; e.g. to get double newlines as a text separator during collection:

;;      (setq register-separator ?+)
;;      (set-register register-separator "\n\n")

;;      ;; URL `https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Registers.html'

;;==============================================;;

(defun sboo-reload-config ()
    "Reload my personal configuration (at `sboo-init-file'). 

Usage:

• Handle a `sigusr2' event.
• Example (triggering):

  $ kill -SIGUSR2 $(pgrep emacs)

Related:

• `special-event-map'"

  (interactive)

  (let* ((CONFIG sboo-init-file)
        )

    (message "Caught « SIGUSR2 » signal. Reloading `sboo-init-file': « %s »" CONFIG)

    (load sboo-init-file)

    t))

;;----------------------------------------------;;

(define-key special-event-map [sigusr1] #'sboo-reload-config)

;; ^ Handle a `sigusr1' event, by reloading the personal configuration.

;;----------------------------------------------;;

(define-key special-event-map [sigusr2] #'keyboard-quit)

;; ^ Handle a `sigusr2' event, by terminating the current computation
;;   (to “thaw” Emacs, if frozen).

;;==============================================;;

(defun sboo-percentage (n)

  "Format the percentage N.

Inputs:

• N — a `numberp' between 0 and 100.

Output:

• a `stringp' of `length' 3 or 4.

Validates N, rounds N."

  (interactive (list (read-number "Percentage: ")))

  (when-let* ((NUMBER (if (numberp n)
                          (round (max 0 (min 100 n)))
                        nil))
              (STRING (format "%02d%%" NUMBER))
              )

    (when (called-interactively-p 'interactive)
      (message "%s" STRING))

    STRING))

;;==============================================;;

(defun sboo-string-all-uppercase-p (string)

  "Return non-nil iff STRING is all capital (ASCII) letters."

  (save-match-data
    (let ((case-fold-search nil)
          )
      (string-match "\\`[A-Z]+\\'" string))))

;; ^ “Wrap the `string-match' call in a `save-match-data' (so any existing regex match data doesn't get overwritten)”.

;;==============================================;;

(defun explog-desaturate-color (color-hex)

  "Converts a color string to its desaturated equivalent hex string.

URL `https://explog.in/notes/poet.html'."

  (require 'color)

  (apply
   'color-rgb-to-hex
   (append (apply
            'color-hsl-to-rgb
            (apply
             'color-desaturate-hsl
             `(,@(apply 'color-rgb-to-hsl (color-name-to-rgb color-hex)) 100)))
           '(2))))

;;----------------------------------------------;;.

(defun explog-transform-theme-colors (fn)

  "Apply FN to the colors on every active face.

FN should accept the face symbol and the current color,
and return the new color to be applied.

URL `https://explog.in/notes/poet.html'."

  (interactive)

  (mapc
   (lambda (face)
     (mapc
      (lambda (attr)
        (let ((current (face-attribute face attr)))
          (unless (or (not current)
                      (listp current)
                      (string= current "unspecified")
                      (string= current "t"))
            (set-face-attribute face nil attr (funcall fn face current)))))
      '(:foreground :background :underline :overline :box :strike-through
                    :distant-foreground))
     (mapc
      (lambda (complex-attr)
        (let* ((full (copy-tree (face-attribute face complex-attr)))
               (current (if (listp full) (member :color full))))
          (unless (or (not current)
                      (not (listp full)))
            (setcar (cdr current) (funcall fn face (cadr current)))
            (set-face-attribute face nil complex-attr full))))
      '(:underline :overline :box)))
   (face-list)))

;;----------------------------------------------;;

(defun explog-desaturate-theme ()

  "As title: desaturate all currently active face colors.

URL `https://explog.in/notes/poet.html'."

  (interactive)

  (explog-transform-theme-colors
   (lambda (face color)
     (explog-desaturate-color color))))

;;----------------------------------------------;;

(defun explog-invert-theme ()

  "Take the complement of all currently active colors.

URL `https://explog.in/notes/poet.html'."

  (interactive)

  (require 'color)

  (explog-transform-theme-colors
   (lambda (face color)
     (apply
      'color-rgb-to-hex
      (color-complement color))))
  (let ((current-ns-appearance (assoc 'ns-appearance default-frame-alist)))
    (cond ((eq (cdr current-ns-appearance) 'light)
           (setf (cdr current-ns-appearance) 'dark))
          ((eq (cdr current-ns-appearance) 'dark)
           (setf (cdr current-ns-appearance) 'light)))))

;;==============================================;;

(defun sboo-flyspell-disable-globally ()

  "Globally disable `flyspell-mode'."

(dolist (BUFFER (buffer-list))
  (with-current-buffer BUFFER
    (flyspell-mode-off))))

;;==============================================;;

(require 'sboo-dwim)

;;==============================================;;

(define-button-type 'help-xref

  'action      #'help-button-action

  'follow-link t)

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