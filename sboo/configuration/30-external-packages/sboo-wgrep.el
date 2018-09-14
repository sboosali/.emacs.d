;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `wgrep': "Writeable GREP"
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wgrep

  ;; :bind (:map grep-mode-map
  ;;             ;; ^
  ;;             ;; NOTE **not** `wgrep-mode-map', which binds:
  ;;             ;;
  ;;             ;;     ("C-x C-q" . wgrep-exit)
  ;;             ;;              
  ;;             ("C-x C-q" . wgrep-toggle-readonly-area)
  ;;             ;; ^
  ;;             ;; the standard keybinding for `toggle-read-only'.
  ;;             ;; for consistency, e.g. with `wdired'.
  ;;             ;;
  ;;             ;; [TODO doesn't work]
  ;;             )

  :config

  (setq

   wgrep-auto-save-buffer t
   ;; ^ save all edited buffers automatically, after `wgrep-finish-edit'.

   wgrep-enable-key "r"
   ;; ^ the key binding to switch to wgrep.

   ))

;; ^ 
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; KeyBindings:
;;
;; You can edit the text in the grep buffer after typing C-c C-p. After that the changed text is highlighted. The following keybindings are defined:
;;
;; C-c C-e: Apply the changes to file buffers.
;; C-c C-u: All changes are unmarked and ignored.
;; C-c C-d: Mark as delete to current line (including newline).
;; C-c C-r: Remove the changes in the region (these changes are not applied to the files. Of course, the remaining changes can still be applied to the files.)
;; C-c C-p: Toggle read-only area.
;; C-c C-k: Discard all changes and exit.
;; C-x C-q: Exit wgrep mode.
;;
;; i.e.:
;;
;; ("\C-c\C-c" . 'wgrep-finish-edit)
;; ("\C-c\C-d" . 'wgrep-mark-deletion)
;; ("\C-c\C-e" . 'wgrep-finish-edit)
;; ("\C-c\C-p" . 'wgrep-toggle-readonly-area)
;; ("\C-c\C-r" . 'wgrep-remove-change)
;; ("\C-x\C-s" . 'wgrep-finish-edit)
;; ("\C-c\C-u" . 'wgrep-remove-all-change)
;; ("\C-c\C-[" . 'wgrep-remove-all-change)
;; ("\C-c\C-k" . 'wgrep-abort-changes)
;; ("\C-x\C-q" . 'wgrep-exit)
;; 

;; See:
;;     - https://github.com/mhayashi1120/Emacs-wgrep
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-wgrep)