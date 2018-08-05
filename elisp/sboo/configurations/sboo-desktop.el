;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DESKTOP
;; 
;; configure the `desktop` builtin-package.
;;
;; (Multiple) Desktop "Sessions"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package desktop

  ;;TODO

  ;; :init
  ;; (sboo-desktop-init)
  
  ;; :config
  ;; (sboo-desktop-config)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-desktop-directory
  (if (fboundp 'sboo-database-file)
      (sboo-database-file "desktop" "")
    user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-desktop-save ()
    "
    "
    (interactive)
    (progn

      (if (eq (desktop-owner) (emacs-pid))
          (let ((desktop-save 'ask-if-new))
            ;; ^ `ask-if-new' means "ask if no desktop file exists, otherwise just save.",
            ;; which `desktop-save' reads.
            ;;
            ;; i.e. only prompt-the-user-for-confirmation if:
            ;; [1] we're saving to a different location
            ;;     (in which case, `desktop-dirname' and/or `desktop-base-file-name' may have not been configured properly); or
            ;; [2] we're saving over a desktop-file that's "unreleased"(?), or that's been updated
            ;;     (e.g. by another emacs application) while the current emacs application has been running
            ;;     (in particular, an "external" `desktop-save' has been called
            ;;      after the most recent "interal" `desktop-read').
            ;;
            ;; NOTE `elisp' has dynamic-scoping, thus `let' correctly shadows any default value.
            ;;
            ;; NOTE `elisp' is a "Lisp2", i.e. variables and functions have different namespaces,
            ;; thus `let' doesn't affect the definition (or boundedness) of any function.
            ;;
            (desktop-save sboo-desktop-directory)))
          ;; ^ 
          ;; `(desktop-save DIRNAME &optional RELEASE ONLY-IF-CHANGED VERSION)`
          ;;
    t))
    ;; ^ in `sboo-quitting', we register `sboo-desktop-save` as a hook to be run when quitting emacs. 
    ;; emacs aborts the quit (i.e. stays open) if any of these "quit hooks" return `nil'.
    ;; NOTE currently, the constant-true return-value means that we don't do any checks, or prompt the user for any input.

;; ^
;;
;; "Don't call `desktop-save-in-desktop-dir', as it prints a message."
;; 
;; See 
;;     https://www.emacswiki.org/emacs/Desktop
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-desktop-read ()
    "
    "
    (interactive)
    (desktop-read))
;; ^ "unconfigured" `desktop-read' works if `desktop-save' was properly "configured".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-config-desktop ()
  "
  "
  (interactive)

  (setq

   desktop-dirname sboo-desktop-directory
   ;; ^

   desktop-restore-eager 100
   ;; ^ Specify the maximum number of buffers to restore immediately;
   ;; the remaining buffers are restored lazily, when Emacs is idle.
   ;; 
   
   desktop-load-locked-desktop t
   ;; ^ `t` means "load the desktop (on startup) without asking"
   
   desktop-auto-save-timeout 5
   ;; ^ unit-of-time is seconds.
   ;; (NOTE the auto-saves are saved to a separate file).

   desktop-path (list sboo-desktop-directory))
   ;; ^ 

  (progn
    (add-to-list 'desktop-modes-not-to-save 'dired-mode)
    (add-to-list 'desktop-modes-not-to-save 'Info-mode)
    (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode))
    ;; ^ You can specify buffers which should not be saved, by name or by mode.

  (progn  
    ;(add-to-list TODO 'sboo-desktop-save)
    ;; ^ 
    (add-hook 'auto-save-hook 'sboo-desktop-save))
    ;; ^ 

  (progn
    (desktop-read sboo-desktop-directory)
    ;; ^
    (desktop-save-mode 1)))
    ;; ^ (enable a mode after configuring its variables).

;; TODO auto-save this every so-often (or on  buffer create/delete change events).
; (desktop-save sboo-desktop-directory)
; ;; ^

;; TODO `desktop-save` on emacs exit (i.e. `C-x C-c`).
;; (desktop-save (sboo-database-file "desktop" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `desktop-path`
;;
;; default value is `("~/.emacs.d/" "~")`
;;
;; > List of directories to search for the desktop file.
;; > The base name of the file is specified in ‘desktop-base-file-name’.
;;

;; `desktop-base-file-name`
;;
;; default value is `".emacs.desktop"`
;;
;; > Name of file for Emacs desktop, excluding the directory part.
;;

;; `desktop-load-locked-desktop`:
;; 
;; > The file in which Emacs saves the desktop is locked while the session runs, to avoid inadvertently overwriting it from another Emacs session. That lock is normally removed when Emacs exits, but if Emacs or your system crashes, the lock stays, and when you restart Emacs, it will by default ask you whether to use the locked desktop file. 
;; > You can avoid the question by customizing the variable desktop-load-locked-desktop to either nil, which means never load the desktop in this case, or t, which means load the desktop without asking.
;; 

;; other `desktop-*` functions and variables:
;;
;; desktop-read
;; desktop-save
;; desktop-save-mode
;; desktop-save-hook
;; desktop-enable 
;; desktop-base-file-name
;; desktop-auto-save-timeout
;; desktop-auto-save-timer
;; desktop-buffers-not-to-save
;; desktop-files-not-to-save
;; desktop-modes-not-to-save
;; desktop-path
;; desktop-restore-eager
;; desktop-restore-frames
;; desktop-load-locked-desktop
;;
;; NOTE
;; 
;; To manually interact with your desktop session at any time, use:
;; - the command ‘M-x desktop-save’ to save it.
;; - the command ‘M-x desktop-read’ to load it.
;;
;; 
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the `desktop+` package:
;; 
;; (require 'desktop+)
;; (defconst sboo-desktop-name-default
;;  "sboo")
;; (desktop-create sboo-desktop-name-default)
;; (desktop+-load   sboo-desktop-name-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the `desktop` lock-file holds the PID of the Emacs process that called `desktop-save` on the lock-file's directory.
;;
;; e.g.
;; 
;; $ cat ~/.emacs.d/.emacs.desktop.lock
;; No such file or directory
;; 
;; M-x desktop-save
;; 
;; $ cat ~/.emacs.d/.emacs.desktop.lock
;; 31194
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `desktop-save`
;;
;; (desktop-save DIRNAME &optional RELEASE)
;;
;; Save the desktop in a desktop file.
;;
;; Parameter DIRNAME specifies where to save the desktop file.
;; Optional parameter RELEASE says whether we’re done with this desktop. 
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-desktop)