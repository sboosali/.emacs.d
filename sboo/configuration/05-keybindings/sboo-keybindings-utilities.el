;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities.
;; 
;; provides utility functions like `sboo-utilities',
;; but only for my (global) keybindings.
;; why? to keep `sboo-keybindings' focused, for readability;
;; its expressions are only the keybindings themselves (no lambdas).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'shell)

;;TODO rm from core utilities:
(require 'dash)           ;; (the `-` prefix)
(require 's)              ;; `s`trings
(require 'f)              ;; `f`iles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Custom Functions, Bound By My Keybindings) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-buffers-list ()

  "Try `helm-buffers-list', fallback to `list-buffers'.
  
  (When `helm' isn't loaded/installed, this command falls back 
  to the standard-library command upon which that package improves.)
  "
  (interactive)

  (if (and (commandp 'helm-buffers-list) 
           (fboundp 'helm-buffers-list))
      ;; ^ NOTE 
      ;; do predicates succeed when command is to-be-`autoload'ed?
    
    (call-interactively #'helm-buffers-list)

   (call-interactively #'list-buffers)))

;;  (if (fboundp 'helm-buffers-list)
;;  (if (featurep 'helm)
;;    (helm-buffers-list)  ;TODO; preserve abilit to autoload
;;   (list-buffers)))
;;    (call-interactively #'helm-buffers-list)
;;   (call-interactively #'list-buffers)))

;; ^
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; ^ see http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-split-window-left-right ()
  ".
  "
  (interactive)
  
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-next-buffer)
  (other-window 1)

)
;; ^

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;TODO doesn't work.
;; (defun redo (&optional arg)
;;   "Redo some previouly-undone changes.
;; Use **`undo`**, not this function, to continue the `redoing`.
;; A numeric ARG serves as a repeat count."
;;   (interactive "*P")
;;   (keyboard-quit)
;;   (undo arg)
;;   )
;; ;; ^ see `undo` in `simple.el`.
;; ;; see https://stackoverflow.com/questions/3527142/how-do-you-redo-changes-after-undo-with-emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell / Terminal.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
(defun sboo-launch-shell ()
  " switch-to-or-create a `shell-mode` buffer.
  "
  (interactive)

  (let*
      ((NAME "*shell*"))
    (if (bufferp (get-buffer NAME))
        (switch-to-buffer (get-buffer NAME) nil 'force-same-window)
      (shell NAME))))

  ;;OLD
  ;; (let*
  ;;     ((n "*shell*")
  ;;      (b (get-buffer n)))
  ;;   (if (bufferp b)
  ;;       (switch-to-buffer b nil 'force-same-window)
  ;;     (shell n))))

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

(defun sboo-launch-term ()
  " switch-to-or-create a `term-mode` buffer.
  "
  (interactive)
   (term "/bin/bash"))

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
(provide 'sboo-keybindings-utilities)