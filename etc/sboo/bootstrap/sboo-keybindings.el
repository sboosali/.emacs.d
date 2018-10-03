;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My global keybindings
;;
;; `sboo-keybindings' binds many commands in `sboo-commands'.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'shell)

;;;(require 'comint)

(require 'sboo-utilities)
(require 'sboo-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Un-Set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; unset some keybindings that are set-by-default.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-unset-key (kbd "<f2>"))

;; ^ by default, <f2> seems to be like C-x, i.e. a *prefix* key.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The "Super"-Modifier ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Management/Navigation for Buffers/Windows/Frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "s-o") #'other-window)       ;TODO;
(global-set-key (kbd "s-s") #'sboo-launch-shell)
(global-set-key (kbd "s-t") #'sboo-launch-term)
(global-set-key (kbd "s-h") #'sboo-split-window-left-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting Unicode characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "s-,") #'sboo-insert-angle-quote-left)
(global-set-key (kbd "s-.") #'sboo-insert-angle-quote-right)
(global-set-key (kbd "s-=") #'sboo-insert-triple-equals-sign)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SingleCharacter-Keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "TAB") 'dabbrev-expand)

;; ^ 
;; `(kbd "TAB")`, *not* `(kbd "<tab>")`.
;;
;; this distinction is necessary to support tab-as-emacs-completion in all buffers and by default (including `shell-mode`),
;; while still supporting tab-as-bash-completion in a terminal buffer (e.g. `term-mode`).
;;
;; globally, "<tab>" always becomes "TAB" ("translated from"), then "TAB" becomes `dabbrev-expand` ("is bound to").
;; locally, in `term-mode-map` (see `sboo-term`) "<tab>" always becomes "TAB" ("translated from"), then "TAB" becomes `self-insert-command` ("is bound to").
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation Keys (so-called) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<print>") #'kill-ring-save)

;;;(global-set-key (kbd "<>") #')
;;;(global-set-key (kbd "<>") #')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<insert>") #'yank)

;;;(global-set-key (kbd "<deletechar>") #')

;;;(global-set-key (kbd "<home>") #')
;;;(global-set-key (kbd "<end>")  #')

;;;(global-set-key (kbd "<proir>") #') ;; i.e. PageUp.
;;;(global-set-key (kbd "<next>")  #') ;; i.e. PageDown.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KeyPad (a.k.a NumPad) Keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<kp-insert>") 'helm-buffer-list)

  ;; ^ alternatives:
  ;;
  ;; - buffer-list
  ;; - electric-buffer-list
  ;; - helm-buffers-list
  ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<kp-enter>") #'helm-find-files)

  ;; ^ alternatives:
  ;;
  ;; - find-file
  ;; - find-file-existing
  ;; - find-file-at-point
  ;; - helm-find-files
  ;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO BROKEN (global-set-key (kbd "<kp-add>") 'sboo-projectile-find-file)

  ;; ^ alternatives:
  ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<kp-subtract>") 'projectile-grep)

  ;; ^ 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<kp-multiply>") 'sboo-launch-shell)

  ;; ^ TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<kp-divide>") 'sboo-launch-term)

  ;; ^ 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<kp-delete>") 'set-mark-command)

  ;; ^ alternatives:
  ;; 
  ;; - `flycheck-list-errors'
  ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; F-KEYS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <f1>..<f4>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(global-set-key (kbd "<f1>") #')

;; ^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f2>") #'sboo-search)

;; ^ like C-s
;;
;; (isearch-forward &optional REGEXP-P NO-RECURSIVE-EDIT)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; <f3> is 'kmacro-start-macro-or-insert-counter
;; <f4> is 'kmacro-start-macro-or-insert-counter

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <f5>..<f8>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f5>") #'sboo-buffers-list)

;; ^ like [C-x C-b]
;;
;; (but not the default behavior/keybinding).
;;
;;
;; alternatives:
;;
;; - `sboo-buffers-list'
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f6>") #'sboo-toggle-buffer)

;; ^ like M-` within some applications (alluding to M-<tab>).
;; 
;; why custom? because `previous-buffer` doesn't have the desired behavior.
;; it doesn't "toggle" between the two most recent windows.
;; its default keybinding: [C-x <left>]
;;
;; alternatives:
;;
;; - `switch-to-previous-buffer'
;; - `purpose-switch-buffer-with-purpose' TODO
;; -
;;

;;;
;;;(global-set-key (kbd "<f6>") #'delete-other-windows)
;;
;; ^ i.e. [C-x 1]
;;
;; (delete-other-windows &optional WINDOW)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f7>") #'xah-prior-user-buffer)

;; ^ 
;; 
;; 
;; alternatives:
;;
;; - `previous-buffer'
;; - `xah-prior-user-buffer'
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f8>") #'xah-next-user-buffer) 

;; ^ 
;; 
;; 
;; alternatives:
;;
;; - `next-buffer'
;; - `xah-prior-user-buffer'
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <f9>..<f12>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f9>") #'undo)

;; ^ 
;;
;; (undo &optional ARG)
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f10>") "\C-g") ;TODO;  [remap ???]

;; ^ i.e. `keyboard-quit`
;;
;; NOTE binding directly to the command `keyboard-quit` doesn't work.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(global-set-key (kbd "<f10>") #'repeat-complex-command)
;;
;; ^ like [M-x <up> <ret>]
;;
;; prompts you with "Redo: ...", in place of "M-x ...".
;; see https://stackoverflow.com/questions/275842/is-there-a-repeat-last-command-in-emacs
;; "similar to M-x M-p, except that repeat-complex-command repeats previous arguments."
;;
;; alternatives:
;;
;; - `repeat-complex-command'
;; -
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f11>") #'pp-eval-expression) 

;; ^ like [ M-: ], a.k.a. `eval-expression`.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f12>") #'sboo-M-x)

;; ^ M-x, a.k.a.'execute-extended-command
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The "Meta"-Modifier ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-a") 'mark-whole-buffer-buffer)

;; ^ i.e. SelectAll, 
;;
;; with "standard"-keybinding (under `<meta>', not `<ctrl>').
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\M-r" 'query-replace-regexp)

;; ^ i.e. FindReplace,
;;
;; with "standard"-keybinding (under `<meta>', not `<ctrl>').
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO rm; (global-set-key "\M-`" 'previous-buffer)

;; ^ 
;; Mnemonic: it's like "ALT-TAB".
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Control-Modifier ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x f")   #'sboo-find-file)
(global-set-key (kbd "C-x C-f") #'sboo-find-file)
;; ^ `ffap': find-file-at-point

(global-set-key (kbd "C-o") 'other-window)

(global-set-key (kbd "C-;") 'comment-region)
;; ^ 

;;;(global-set-key (kbd "C-x C-o") 'other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Miscellaneous Keybindings) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-M-m") 'maximize-frame)

;; ^ 
;; <f11> is a *system*-global hotkey for window-maximizing;
;; since we overrode it, let's replace it.
;;

;;; (global-set-key (kbd "<f9>") 'pop-tag-mark)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DEFINING KEYBINDINGS
;; ====================
;;
;; http://ergoemacs.org/emacs/keyboard_shortcuts.html
;; 
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Changing-Key-Bindings.html
;; 
;; Problematc KeyBindings:
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; • 【F1】 or 【Ctrl+h】
;; 
;; (This key is used for emacs help system and have a special status in emacs's key system.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; • the «ESC» key — 【Ctrl+[】
;; 
;; The Escape key is tied to 【Ctrl+[】 and Meta.
;; Escape by itself has complicated meanings depending when it is pressed and how many times it is pressed.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; • the «RET» key — 【Ctrl+m】
;;  
;; 【Ctrl+m】 and Enter are tied together.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; • the «TAB» key — 【Ctrl+i】
;; 
;; 【Ctrl+i】 and Tab are tied together.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; •【Ctrl+0】 to 【Ctrl+9】—【Alt+0】 to 【Alt+9】
;; 
;; By default, they are bound to `digit-argument'.
;; 
;; You can rebind them, and use `universal-argument' 【Ctrl+u】 instead.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; • «Hyper» and/or «Super»
;; 
;; You can set them to any of:
;;
;; - « ❖ Window »
;; - « ▤ Menu »
;; - « ⌥ option »
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;   (`define-key' Keymap Key Binding)

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
(provide 'sboo-keybindings)