;;; -*- lexical-binding: t -*-

;;; Commentary:

;;----------------------------------------------;;
;; Configuration for the `desktop' (builtin) package.
;;
;; TODO: (Multiple) Desktop "Sessions"
;;
;;----------------------------------------------;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtin packages:

(require 'cl)
(require 'pcase)

(require 'desktop)

;; sboo packages:

(require 'sboo-xdg nil :noerror)

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-desktop-directory

  (if (require 'sboo-xdg nil :noerror)

      (sboo-xdg-data "" :subdir "emacs/desktop")

    emacs-directory)

  "`desktop-dirname' under « $XDG_DATA_DIR ».")

;; ^ `desktop-dirname' is `~/.emacs.d' by default.

;;----------------------------------------------;;

(defvar sboo-desktop-file

  (concat (file-name-as-directory sboo-desktop-directory)
          (concat desktop-base-file-name ".el"))

  "`desktop-base-file-name' under « $XDG_DATA_DIR ».")

;; ^ `desktop-dirname' is `~/.emacs.d' by default.

;;----------------------------------------------;;
;;; Functions ----------------------------------;;
;;----------------------------------------------;;
;;; `:init'      (sboo-xdg-data "" :subdir "emacs/desktop")


(defun sboo-desktop-init! ()

  "Initialize `desktop-mode' variables."
  (interactive)

  (setq desktop-dirname sboo-desktop-directory)
  ;; ^

  (setq desktop-restore-eager 100)
  ;; ^ The maximum number of buffers to restore immediately;
  ;; the remaining buffers are restored lazily (when Emacs is idle).
  ;;
  ;; NOTE `10' is instant, `100' takes a few seconds.

  (setq desktop-load-locked-desktop t)
  ;; ^ `t' means "load the desktop (on startup) without asking"

  (setq desktop-auto-save-timeout 30)
  ;; ^ Unit-Of-Time is seconds.
  ;;
  ;; (NOTE the auto-saves are saved to a separate file). [TODO i.e.?]

  (setq desktop-path (list sboo-desktop-directory))
  ;; ^ 

  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  ;; ^ Buffers which should not be saved, either by mode or by name.

  ())

;;----------------------------------------------;;
;;; `:config'

(defun sboo-desktop-config! ()

  "Load (`desktop-read') and enable (`desktop-save-mode')."

  (interactive)

  (make-directory (file-name-directory sboo-desktop-directory)
                  :create-parent-directories)

  (desktop-read)

  ;; ^
  ;; 

  (desktop-save-mode +1)

  ;; ^ Enable globally.

  ())

;TODO `desktop-save' on emacs exit (i.e. `C-x C-c`).

;; Notes:
;;
;; (desktop-save DIRNAME :release :only-if-changed "...")
;;
;; e.g. « (desktop-save sboo-desktop-directory) »
;;

;;----------------------------------------------;;

(defun sboo-desktop-save ()
 
  "The function `desktop-save' for the feature `sboo-desktop'."

  (interactive)

  (let ((RELEASE         nil)
        (ONLY-IF-CHANGED t)
        (VERSION         nil)
        )

  (desktop-save sboo-desktop-directory RELEASE ONLY-IF-CHANGED VERSION)))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;;; `desktop.el' Documentation
;; 
;; 
;;

;; `desktop-save':
;;
;;     (desktop-save DIRNAME &optional RELEASE ONLY-IF-CHANGED VERSION)
;;

;;----------------------------------------------;;

;; `desktop-path':
;;
;; default value is `("~/.emacs.d/" "~")`
;;
;; > List of directories to search for the desktop file.
;; > The base name of the file is specified in ‘desktop-base-file-name’.
;;

;; `desktop-base-file-name':
;;
;; default value is `".emacs.desktop"`
;;
;; > Name of file for Emacs desktop, excluding the directory part.
;;

;; `desktop-load-locked-desktop':
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
	
;;----------------------------------------------;;

;; the `desktop+` package:
;; 
;; (require 'desktop+)
;; (defconst sboo-desktop-name-default
;;  "sboo")
;; (desktop-create sboo-desktop-name-default)
;; (desktop+-load   sboo-desktop-name-default)

;;----------------------------------------------;;

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

;;----------------------------------------------;;

;; `desktop-save`
;;
;; (desktop-save DIRNAME &optional RELEASE)
;;
;; Save the desktop in a desktop file.
;;
;; Parameter DIRNAME specifies where to save the desktop file.
;; Optional parameter RELEASE says whether we’re done with this desktop. 
;;

;;----------------------------------------------;;

 ;;  (defun sboo-desktop-owner-advice (original &rest args)

 ;; "Whether a pid is the Process ID of an Emacs process. (Also returns nil if pid is nil.)
  
 ;;  (should be) Cross-Platform.

 ;;  By sylvain, see https://www.emacswiki.org/emacs/Desktop

 ;;  "
 ;;  (interactive)

 ;;    (let ((owner (apply original args)))
 ;;      (if (and owner (/= owner (emacs-pid)))
 ;;          (and (car (member owner (list-system-processes)))
 ;;               (let (cmd (attrlist (process-attributes owner)))
 ;;                 (if (not attrlist) owner
 ;;                   (dolist (attr attrlist)
 ;;                     (and (string= "comm" (car attr))
 ;;                          (setq cmd (car attr))))
 ;;                   (and cmd (string-match-p "[Ee]macs" cmd) owner))))
 ;;        owner)))

 ;;  ;; ^
 ;;  ;;
 ;;  ;; (because Windows lacks `process-attributes’?)
 ;;  ;;
 ;;  ;; Ensure that dead system processes don't own it.
 ;;  ;;

 ;;  (advice-add #'desktop-owner :around #'sylvain/desktop-owner-advice)


 ;;  ;; ^ `advice` supports modification without redefinition.
 ;;  ;; 
 ;;  ;; e.g. you need to modify a function defined in another library, or you need to modify a hook like foo-function, a process filter, or basically any variable or object field which holds a function value.
 ;;  ;; 
 ;;  ;; 
 ;;  ;;
 ;;  ;; See
 ;;  ;;     - https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
 ;;  ;;

;;----------------------------------------------;;

;; e.g.
;; Automatically save and restore sessions:
;;
;;     (progn
;;       (setq desktop-dirname              "~/.emacs.d/desktop/"
;;             desktop-base-file-name       "emacs.desktop"
;;             desktop-base-lock-name       "lock"
;;             desktop-path                 `(,desktop-dirname)
;;             desktop-save                 t    ;; always save.
;;             desktop-files-not-to-save    "^$" ;; reload tramp paths.
;;             desktop-load-locked-desktop  nil  ;; don’t load.
;;             desktop-auto-save-timeout    30)  ;; Number of seconds of idle time before auto-saving the desktop.
;;                                               ; The desktop will be auto-saved when this amount of idle time have
;;                                               ; passed after some change in the window configuration.
;;       (desktop-save-mode 1))
;;

;;----------------------------------------------;;

;;; Links
;;
;;     - https://www.emacswiki.org/emacs/Desktop
;;     - 
;;     - 
;;     - 

;;----------------------------------------------;;
(provide 'sboo-desktop)