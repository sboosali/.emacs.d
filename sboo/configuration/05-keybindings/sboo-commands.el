;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  `sboo-` and `xah-` commands
;; 
;; See my `defgrace` macro.
;;
;; provides utility functions like `sboo-utilities',
;; but only for my (global) keybindings.
;; why? to keep `sboo-keybindings' focused, for readability;
;; its expressions are only the keybindings themselves (no lambdas).
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
;; (Custom Functions, Bound By My Keybindings) ;;;
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
(provide 'sboo-commands)