;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom `interactive' commands.
;;
;; Most commands are namespaced under `sboo-` or `xah-`
;; (as are their non-`interactive' utilities).
;; 
;; See my `defgrace` macro, for conveniently defining commands with fallbacks
;; (when external packages haven't been installed and/or can't be loaded).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities: Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defgrace (Name ExternalCommand BuiltinCommand &optional DocString)

  `(defun ,Name ()
     
     ,DocString
     
     (interactive)
     
     (let ((*command* (function ,ExternalCommand)))
  
    (if (and (commandp *command*) 
             (fboundp  *command*))
             
      (call-interactively *command*)

     (call-interactively (function ,BuiltinCommand))))))

;; ^ `defalias' for commands with graceful degradation.
;;
;; Wraps `defun' and `call-interactively'.
;;
;; NOTE (function f) is like #'f
;;
;; NOTE the predicates succeed even when command is marked with `autoload'.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cycle Through (& Toggle Between) User Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problems with `next-buffer' / `previous-buffer':
;;
;; (1) you cannot hold down the key for the command to repeat.
;;
;; (2) they will go thru many buffers user are not interested in cycling thru.
;;

;; See:
;;     - http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xah-next-user-buffer ()

  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"

  (interactive)

  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xah-prior-user-buffer ()

  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"

  (interactive)

  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xah-user-buffer-q ()

  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"

  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-toggle-buffer ()

  "Switch to the previously open buffer.
  
  Repeated invocations **toggle** between the two most recently open buffers.
  (c.f. the default behavior of repeated `other-buffer' invocations, 
  which **cycle** through all open buffers).

  See http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
  "
  (interactive)

  (switch-to-buffer 
    (other-buffer (current-buffer) 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Management,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-split-window-left-right ();;TODO

  "
  "

  (interactive)
  
  (progn
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (switch-to-next-buffer)
    (other-window 1)
    ()))

;; ^
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
  (interactive)
  (let ((-path (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2)
                   (setq p0 (point))
                   ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                   (skip-chars-backward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
                   (setq p2 (point))
                   (goto-char p0)
                   (buffer-substring-no-properties p1 p2)))))
    (if (string-match-p "\\`https?://" -path)
        (browse-url -path)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" -path)
            (progn
              (let (
                    (-fpath (match-string 1 -path))
                    (-line-num (string-to-number (match-string 2 -path))))
                (if (file-exists-p -fpath)
                    (progn
                      (find-file -fpath)
                      (goto-char 1)
                      (forward-line (1- -line-num)))
                  (progn
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" -fpath))
                      (find-file -fpath))))))
          (progn
            (if (file-exists-p -path)
                (find-file -path)
              (if (file-exists-p (concat -path ".el"))
                  (find-file (concat -path ".el"))
                (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" -path))
                  (find-file -path ))))))))))

;; ^ this command will:
;; - open the file path under cursor, without confirmation;
;; - and jump to line number, if the path ends with ":<number>".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell / Terminal.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-launch-shell ();;TODO

  "Switch to (or create) a`shell-mode` buffer."

  (interactive)

  (let*
      ((NAME "*shell*"))
    (if (bufferp (get-buffer NAME))
        (switch-to-buffer (get-buffer NAME) nil 'force-same-window)
      (shell NAME))))

  ;; ^
  ;;
  ;; `shell`
  ;; '''Run an inferior shell, with I/O through BUFFER (which defaults to ‘*shell*’).
  ;; If BUFFER exists but shell process is not running, make new shell.
  ;; If BUFFER exists and shell process is running, just switch to BUFFER.'''
  ;;
  ;; `switch-to-buffer`
  ;; (switch-to-buffer BUFFER-OR-NAME &optional NORECORD FORCE-SAME-WINDOW)
  ;; '''Display buffer BUFFER-OR-NAME in the selected window.'''
  ;; NOTE `shell` doesn't have an option for the `'force-same-window` behavior.
  ;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-launch-term ()

  "Switch to (or create) a `term-mode` buffer."

  (interactive)
  
  (progn
  
    (term "/bin/bash") ;;TODO still prompts

    ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filesystem Navigation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-find-file ()

  "Wraps `ffap' ("find file at point").

  TODO handle "dir/dir/file.ext:line:column"
  "
  
  (interactive)

  (if (commandp #'ffap)

      (call-interactively #'ffap)

    (call-interactively #'find-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text Navigation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xah-search-current-word ()

  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)

  (let ( $p1 $p2 )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq $p1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq $p2 (point))))

    (setq mark-active nil)

    (when (< $p1 (point))
      (goto-char $p1))

    (isearch-mode t)

    (isearch-yank-string (buffer-substring-no-properties $p1 $p2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-projectile-find-file ()
  (interactive)
  (projectile-find-file))
  ;;OLD (projectile-find-file (make-hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun sboo-projectile-grep ()
;;   (interactive)
;;   (projectile-grep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unicode insertion.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-insert-angle-quote-left ()
  
  "`insert' \"«\", the \"LEFT-POINTING DOUBLE ANGLE QUOTATION MARK\" Unicode character,
  with spacing."
  (interactive)

  (insert "« "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-insert-angle-quote-right ()
  
  "`insert' \"»\", the \"RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK\" Unicode character,
  with spacing."
  (interactive)
  
  (insert " »"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-insert-triple-equals-sign ()
  
  "`insert' \"≡ \", the \"IDENTICAL TO\" Unicode character,
  with spacing."
  (interactive)
  
  (insert "≡ "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands that Gracefully Degrade ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgrace sboo-buffers-list
          helm-buffers-list
          list-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgrace sboo-M-x
          helm-M-x 
          execute-extended-command)

;;  "Try `helm-M-x', fallback to `execute-extended-command'.  
;;  (When `helm' isn't loaded/installed, this command falls back 
;;  to the standard-library command upon which that package improves.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgrace sboo-search
          xah-search-current-word
          isearch-forward)

;; ^ 
;; i.e. fallback to `isearch-forward'.  
;;
;; alternatives:
;; 
;; - xah-search-current-word
;; - isearch-forward
;; - isearch-forward-regexp
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interactive Commands
;; ====================
;; 
;; 
;; 

;; `completing-read'
;;  ===============
;; 
;;     (completing-read `PROMPT' `COLLECTION'
;;            &optional `PREDICATE' `REQUIRE-MATCH' `INITIAL-INPUT' `HIST' DEF INHERIT-INPUT-METHOD)
;;
;; Read a string in the minibuffer, with completion.
;;
;; `PROMPT' is a string to prompt with; normally it ends in a colon and a space.
;; e.g.
;;     "Symbol: "
;;
;; `COLLECTION' can be:
;; - a list of strings,
;; - an alist,
;; - an obarray,
;; - a hash table,
;; - a function that performs (HOW?) the completion itself.
;;
;; `PREDICATE'
;;
;; `REQUIRE-MATCH'
;;
;; `INITIAL-INPUT'
;;
;; `HIST' 
;;
;; 
;; 
;; 
;; 
;; 
;; 
;; 

;; List Major Modes:
;;
;; You can view a list of major mode names by Alt+x `describe-variable' on `auto-mode-alist':
;; 
;;     (describe-variable 'auto-mode-alist)
;;

;; list-matching-lines
;;
;; ^ M-x `list-matching-lines' shows all lines in a buffer that match some regexp.

;; See:
;;    - http://www.wilkesley.org/~ian/xah/emacs/emacs_open_file_path_fast.html
;;    - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-commands)