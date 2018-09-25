;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My configuration for the `god-mode' package.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package god-mode

  ;; :bind (("<escape>" . god-local-mode)
  ;;        )

  ;; ;; :config
  ;; (progn
  ;;   (setq god-exempt-major-modes nil)
  ;;   (setq god-exempt-predicates nil))

  :commands (god-local-mode god-mode-all))

;; ^ 
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; KeyBindings.
;;
;; Toggle between God mode and non-God mode using ESC:
;;
;; (global-set-key (kbd "<escape>") 'god-local-mode)
;;
;; If you want to enable/disable on all active and future buffers, use this:
;;
;; (global-set-key (kbd "<escape>") 'god-mode-all)
;;

;; Configuration.
;;
;; If you are using the global mode, you might want to make no buffers exempt:
;;
;; (setq god-exempt-major-modes nil)
;; (setq god-exempt-predicates nil)
;;
;; This means that e.g. magit-mode or dired-mode will also enter god-mode when you activate it globally, and vise-verse. It means you can always reliably use god-mode commands in any buffer as long as it is globally activated.
;;

;; External (non-Emacs) Configuration.
;;
;; to rebind `CapsLock' to `Escape' on Linux (X11), add:
;;
;;     remove Lock = Caps_Lock
;;     keysym Caps_Lock = Escape
;;
;; to « ~/.xmodmap ».
;; 

;; Related.
;;
;; `evil-mode'
;;

;; See:
;;     - https://github.com/chrisdone/god-mode/blob/master/README.md
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-god-mode)