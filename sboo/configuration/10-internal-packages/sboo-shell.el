;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SHELL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; the `shell` builtin-package.
;;
;; `shell-mode` *not* a terminal-emulator. 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package shell

  :config

  (push
   (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
   ;; ^
   ;;
   ;; How To Override The Default Behavior Of  "M-x shell" Opening A New Buffer In Another Window (e.g. splitting).
   ;;
   ;; see:
   ;;    https://stackoverflow.com/questions/40301732/m-x-shell-open-shell-in-other-windows
   ;;
   ;; >  The command `shell` uses `pop-to-buffer`.
   ;; > If you have the Emacs source code, you can see it for yourself by running `C-h d f shell` to open the function's documentation (and then clicking the link to the function's source).
   ;; `pop-to-buffer` can be configured via `display-buffer-alist`. 

  :bind

  ( ("C-c s" . shell)
  
    :map shell-mode-map

    ("<kp-prior>" . comint-previous-input)
    ;; ^ <prior> is the page-up key.
    ;; `comint-previous-input` is like "press down in a terminal-emulator".
    
    ("<kp-next>" . 'comint-next-input)
    ;; ^ <next> is the page-down key.
    ;; `comint-next-input` is like "press up in a terminal-emulator".

    ("<tab>" . 'dabbrev-expand)
    ;; ^ `<tab>' preceeds `TAB';
    ;; intentionally shadow the shell's auto-completion (for files, commands, etc)
    ;; with emacs's auto-completion (for anything).

    ))
;; ^

;; TODO `comint-mode-map`???
;; ^ many shell modes inherit from `comint-mode`,
;; like the (simple, built-in) `shell-mode`.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; relevant `shell-*` functions and variables:
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;

;; `shell-mode-map`
;; the `shell-mode` major-mode has the `shell-mode-map` keymap.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-shell)