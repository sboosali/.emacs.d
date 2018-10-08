;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `:init'

(defun sboo-autosave-init! ()
  (interactive)

  (setq auto-save-visited-interval 1)
  ;; ^ autosave each second (by default, 5s).

  (setq auto-save-visited-file-name nil)
  ;; ^ disable a setting which itself disables our feature.

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `:config'

(defun sboo-autosave-config! ()
  (interactive)

  (auto-save-visited-mode 1)
  ;; ^ Autosave by overwriting the visited file.
  ;; 
  ;; `auto-save-visited-mode' is a GlobalMinorMode.
  ;;

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ^ `auto-save-visited-mode':
;;
;; NOTE `auto-save-visited-mode' was introduced in `emacs-major-version' 26.1
;;
;; Unlike `auto-save-visited-file-name', the new `auto-save-visited-mode' mode uses normal saving-procedure, and thus obeys saving-hooks.

;; See:
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Files.html
;; - https://www.reddit.com/r/emacs/comments/7h5til/uelizaretskii_emacs_26_is_nearing_its_release_the/
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-autosave)