;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-utilities "utilities")

(require 'shell)
;;;;(require 'comint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (CUSTOM FUNCTIONS BOUND BY THE KEYBINDINGS BELOW) ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; ^ see http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/

;;;;;;;;;;

;;TODO doesn't work.
(defun redo (&optional arg)
  "Redo some previouly-undone changes.
Use **`undo`**, not this function, to continue the `redoing`.
A numeric ARG serves as a repeat count."
  (interactive "*P")
  (keyboard-quit)
  (undo arg)
  )

;; ^ see `undo` in `simple.el`.
;; see https://stackoverflow.com/questions/3527142/how-do-you-redo-changes-after-undo-with-emacs

;;;;;;;;;;

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

;;;;;;;;;;

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

;;;;;;;;;;

(defun sboo-projectile-find-file ()
  (interactive)
  (projectile-find-file))
  ;;OLD (projectile-find-file (make-hash-table))

;; (defun sboo-projectile-grep ()
;;   (interactive)
;;   (projectile-grep))

;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The "Super"-Modifier ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; i.e. Management / Navigation, for Buffers / Windows / Frames.

(global-set-key (kbd "s-o") 'other-window);;TODO
(global-set-key (kbd "s-s") 'sboo-launch-shell)
(global-set-key (kbd "s-t") 'sboo-launch-term)
(global-set-key (kbd "s-h") 'sboo-split-window-left-right)

;;TODO
;; (global-set-key (kbd "s-s") 'shell)
;; (global-set-key (kbd "s-t") 'term)

;;TODO
;; (defun sboo-launch-shell ()
;;   (switch-to-buffer "*shell*" nil 'force-same-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Un-Set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; i.e. unset some keybindings that are set-by-default.

(global-unset-key (kbd "<f2>"))
;; ^ by default, <f2> seems to be like C-x, i.e. a *prefix* key.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SingleCharacter-Keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "TAB") 'dabbrev-expand)
;;^ `(kbd "TAB")`, *not* `(kbd "<tab>")`.
;;
;; this distinction is necessary to support tab-as-emacs-completion in all buffers and by default (including `shell-mode`),
;; while still supporting tab-as-bash-completion in a terminal buffer (e.g. `term-mode`).
;;
;; globally, "<tab>" always becomes "TAB" ("translated from"), then "TAB" becomes `dabbrev-expand` ("is bound to").
;; locally, in `term-mode-map` (see `sboo-term`) "<tab>" always becomes "TAB" ("translated from"), then "TAB" becomes `self-insert-command` ("is bound to").
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; F-KEYS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; <f1>..<f4>

(global-set-key (kbd "<f1>")  'isearch-forward)
;; i.e. C-s
;; (isearch-forward &optional REGEXP-P NO-RECURSIVE-EDIT)

;; TODO (global-set-key (kbd "<f2>")  ')
;; ^
;; [C-x b] was originally:
;; (switch-to-buffer BUFFER-OR-NAME &optional NORECORD FORCE-SAME-WINDOW)

;; <f3> is 'kmacro-start-macro-or-insert-counter
;; <f4> is 'kmacro-start-macro-or-insert-counter

;;;;;;;;;;;;;;;;;;;;
;; <f5>..<f8>

(global-set-key (kbd "<f5>")  'undo)
;; ^ (undo &optional ARG)

(global-set-key (kbd "<f6>")  'delete-other-windows)
;; ^ i.e. [C-x 1]
;; (delete-other-windows &optional WINDOW)
;; OR 'redo

(global-set-key (kbd "<f7>")  'helm-buffers-list)
;; ^ like [C-x C-b] (but not the default).

(global-set-key (kbd "<f8>")  'switch-to-previous-buffer)
;; ^ like M-` (alluding to M-<tab>) within other applications.
;; 
;; why custom? because `previous-buffer` doesn't have the desired behavior.
;; it doesn't "toggle" between the two most recent windows.
;; its default keybinding: [C-x <left>]

;;;;;;;;;;
;; older versions...

;; (global-set-key (kbd "<f7>")  'split-window-right)
;; ;; ^ i.e. [C-x 3]
;; ;; (split-window-right &optional SIZE)

;; (global-set-key (kbd "<f8>")  'other-window)
;; ;; ^ i.e. [C-x o]
;; ;; (other-window COUNT &optional ALL-FRAMES)

;;;;;;;;;;;;;;;;;;;;
;; <f9>..<f12>

(global-set-key (kbd "<f9>")  "\C-g")
;; ^ i.e. `keyboard-quit`
;; binding directly to the command `keyboard-quit` doesn't work.

(global-set-key (kbd "<f10>") 'pp-eval-expression) 
;; ^ M-:
;; or, `eval-expression`.

(global-set-key (kbd "<f11>") 'repeat-complex-command)
;; ^ like [M-x <up> <ret>]
;; prompts you with "Redo: ...", in place of "M-x ...".
;; see https://stackoverflow.com/questions/275842/is-there-a-repeat-last-command-in-emacs
;; "similar to M-x M-p, except that repeat-complex-command repeats previous arguments."

(global-set-key (kbd "<f12>") 'helm-M-x)
;; ^ M-x
;; a.k.a.'execute-extended-command

;; NOTES:
;; (global-set-key (kbd "<f1>")  ')
;; (global-set-key (kbd "<f2>")  ')
;; (global-set-key (kbd "<f3>")  ')
;; (global-set-key (kbd "<f4>")  ')
;; (global-set-key (kbd "<f5>")  ')
;; (global-set-key (kbd "<f6>")  ')
;; (global-set-key (kbd "<f7>")  ')
;; (global-set-key (kbd "<f8>")  ')
;; (global-set-key (kbd "<f9>")  ')
;; (global-set-key (kbd "<f10>") ')
;; (global-set-key (kbd "<f11>") ')
;; (global-set-key (kbd "<f12>") ')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Weird"-Keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<insert>") 'yank)

(global-set-key (kbd "<print>") 'kill-ring-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NumPad-Keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<kp-insert>") 'helm-buffer-list)
  ;; ^ alternatives:
  ;;
  ;; - buffer-list
  ;; - electric-buffer-list
  ;; - helm-buffers-list
  ;;

(global-set-key (kbd "<kp-enter>") 'helm-find-files)
  ;; ^ alternatives:
  ;;
  ;; - find-file
  ;; - find-file-existing
  ;; - find-file-at-point
  ;; - helm-find-files
  ;; 

;;TODO BROKEN (global-set-key (kbd "<kp-add>") 'sboo-projectile-find-file)
  ;; ^ alternatives:
  ;;

(global-set-key (kbd "<kp-subtract>") 'projectile-grep)
  ;; ^ 

(global-set-key (kbd "<kp-multiply>") 'sboo-launch-shell)
  ;; ^ TODO

(global-set-key (kbd "<kp-divide>") 'sboo-launch-term)
  ;; ^ 

(global-set-key (kbd "<kp-delete>") 'set-mark-command)
  ;; ^ 
;; 'flycheck-list-errors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The "Meta"-Modifier ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\M-r" 'query-replace-regexp)
;; ^ for my own "CUA"-mode.

(global-set-key "\M-`" 'previous-buffer) 
;; ^ mnemonic: it's like ALT-TAB.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Control-Modifier ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Miscellaneous Keybindings) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-M-m") 'maximize-frame)
;; ^ since <f11> is the *system*-global hotkey
;; for window-maximizing, which we overrode.

; (global-set-key (kbd "<f9>") 'pop-tag-mark)

(global-set-key (kbd "C-o") 'other-window)
;;;(global-set-key (kbd "C-x C-o") 'other-window)

(global-set-key (kbd "C-;") 'comment-region)
;; ^
;; `comment-region`
;;
;; M-x comment-region
;; C-c C-c (in C-like modes)
;; Add comment delimiters to all the lines in the region.
;; 
;; See
;;     - https://www.gnu.org/software/emacs/manual/html_node/emacs/Comment-Commands.html
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard-Specific ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<apps>") 'helm-buffers-list)
(global-set-key (kbd "<menu>") 'helm-buffers-list)
;; ^ Windows(-keyboard)-specific.
;;
;; both keys, "<apps>" and "<menu>" looked the same key;
;; same location/icon, on different keyboards.
;;
;; the <apps> key looks like: a rectangle; a mouse clicking on a menu item of a menu.
;; it's on right at the bottom row.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Platform-Specific ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE
;; <f1> can't be an emacs shortcut, 
;; when run with Dragon NaturallySpeaking (on Windows).
;; <f1> is Dragon NaturallySpeaking shortcut, which 
;; crashes when I try to edit the shortcuts.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Menu-Bar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. <menu-bar> <edit> <paste>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Tool-Bar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. ???

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODAL: SHELL-MODES

;; (require 'eshell)
;; (progn
;;   (define-key eshell-mode-map
;;     (kbd "<kp-prior>") 'eshell-previous-input)
;;   (define-key eshell-mode-map
;;     (kbd "<kp-next>") 'eshell-next-input))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ======================
;; NumPad-Keys
;; ======================
;; <kp-0>         | "0"
;; <kp-1>         | "1"
;; <kp-2>         | "2"
;; <kp-3>         | "3"
;; <kp-4>         | "4"
;; <kp-5>         | "5"
;; <kp-6>         | "6"
;; <kp-7>         | "7"
;; <kp-8>         | "8"
;; <kp-9>         | "9"
;; <kp-add>       | "+"
;; <kp-begin>     | "<begin>"
;; <kp-decimal>   | "."
;; <kp-delete>    | "<deletechar>"
;; <kp-divide>    | "/"
;; <kp-down>      | "<down>"
;; <kp-end>       | "<end>"
;; <kp-enter>     | "RET"
;; <kp-equal>     | "="
;; <kp-home>      | "<home>"
;; <kp-insert>    | "<insert>"
;; <kp-left>      | "<left>"
;; <kp-multiply>  | "*"
;; <kp-next>      | "<next>"
;; <kp-prior>     | "<prior>"
;; <kp-right>     | "<right>"
;; <kp-separator> | ","
;; <kp-space>     | "SPC"
;; <kp-subtract>  | "-"
;; <kp-tab>       | "TAB"
;; <kp-up>        | "<up>"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-keybindings)