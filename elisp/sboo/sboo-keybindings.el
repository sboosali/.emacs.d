;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS

(require 'sboo-utilities "utilities")

(require 'shell)
;;;;(require 'comint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS BOUND BY THE KEYBINDINGS BELOW

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNSET

;; by default, <f2> is like C-x, i.e. a *prefix* key.
(global-unset-key (kbd "<f2>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SINGLE-CHARACTER

(global-set-key (kbd "<tab>") 'dabbrev-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; F-KEYS

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

(global-set-key (kbd "<f7>")  'electric-buffer-list)
;; ^ like [C-x C-b] (but not the default).
;; (electric-buffer-list ARG)

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

(global-set-key (kbd "<f12>") 'execute-extended-command)
;; ^ M-x

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NUMPAD

;(global-set-key (kbd "<kp-insert>") 'electric-buffer-list) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; META-MODIFIER

(global-set-key "\M-r" 'query-replace-regexp)
;; ^ for my own "CUA"-mode.

(global-set-key "\M-`" 'previous-buffer) 
;; ^ mnemonic: it's like ALT-TAB.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONTROL-MODIFIER

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISCELLANEOUS

(global-set-key (kbd "C-M-m") 'maximize-frame)
;; ^ since <f11> is the *system*-global hotkey
;; for window-maximizing, which we overrode.

; (global-set-key (kbd "<f9>") 'pop-tag-mark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBOARD-SPECIFIC 

(global-set-key (kbd "<pause>")        'set-mark-command)

(global-set-key (kbd "<apps>") 'execute-extended-command)
;; ^ Windows(-keyboard)-specific.
;; the <apps> key looks like: a rectangle; a mouse clicking on a menu item of a menu.
;; it's on right at the bottom row.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLATFORM-SPECIFIC 

;; NOTE
;; <f1> can't be an emacs shortcut, 
;; when run with Dragon NaturallySpeaking (on Windows).
;; <f1> is Dragon NaturallySpeaking shortcut, which 
;; crashes when I try to edit the shortcuts.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MENU-BAR

;; e.g. <menu-bar> <edit> <paste>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOOL-BAR

;; e.g. ???

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODAL: SHELL-MODE



(progn

  ;; the `shell-mode` major-mode has 
  ;; the `shell-mode-map` keymap.

  ;;;;(define-key comint-mode-map (kbd "<kp-prior>") 'comint-previous-input)
  (define-key  shell-mode-map (kbd "<kp-prior>") 'comint-previous-input)
  ;; <prior> is the page-up key

  ;;;;(define-key comint-mode-map  (kbd "<kp-next>") 'comint-next-input)
  (define-key  shell-mode-map  (kbd "<kp-next>") 'comint-next-input)
  ;; <next> is the page-down key

)

;; `comint-mode-map`
;; ^ many shell modes inherit from `comint-mode`,
;; like the (simple, built-in) `shell-mode`.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MODAL: 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NOTES

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-keybindings)