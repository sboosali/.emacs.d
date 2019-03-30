;;; -*- lexical-binding: t -*-

;;==============================================;;
;;; Commentary:

;; Configuration for `desktop-mode'.
;;
;; Resources (files):
;;
;; • « "${XDG_DATA_HOME}/emacs/desktop/.emacs.desktop" »
;;
;; Resources (environment variables):
;;
;; • « $SBOO_EMACS_DESKTOP_ENABLE »
;; • « $SBOO_EMACS_DESKTOP_RESTORE_EAGER »
;;
;; 
;;
;; TODO: (Multiple) Desktop "Sessions"

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtin packages:

(require 'cl-lib)
(require 'pcase)

(require 'desktop)

;; sboo packages:

(require 'sboo-xdg nil :noerror)

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-star-buffer-regex "\\`\\*.*\\*\\'"

  "Regex which matches a Star Buffer.

Star Buffers are Emacs' naming convention for Internal Buffers.")

;;----------------------------------------------;;

(defun sboo-bury-buffer-if-star-buffer (buffer)

  (when (string-match-p sboo-star-buffer-regex (buffer-name buffer))
    (bury-buffer buffer)))

;;----------------------------------------------;;

(defun sboo-bury-all-star-buffers ()

  "Bury all star buffers.

URL `http://emacs.stackexchange.com/a/20036/115'."

  (interactive)

  (mapcar #'sboo-bury-buffer-if-star-buffer
          (buffer-list)))

;; `bury-buffer':
;;
;;(bury-buffer &optional BUFFER-OR-NAME)
;;
;;Put BUFFER-OR-NAME at the end of the list of all buffers.
;;There it is the least likely candidate for ‘other-buffer’ to return.
;;
;; 

;;----------------------------------------------;;

(defun sboo-desktop-enable-p ()

  "Whether `desktop-mode' restores on startup.

Output:

• an `booleanp'.

Resources:

• the « $SBOO_EMACS_DESKTOP_ENABLE » environment variable."

  (let* ((VALUE-DYNAMIC (condition-case nil
                            (sboo-getenv-boolean "SBOO_EMACS_DESKTOP_ENABLE")
                          (error nil)))
         (VALUE-STATIC  t)
         )

    (or (if VALUE-DYNAMIC t nil)
        VALUE-STATIC)))

;;----------------------------------------------;;

(defun sboo-desktop-restore-eager ()

  "How many buffers should `desktop' restore on startup.

Output:

• an `integerp'.

Notes:

• « 0 » is fastest but least convenient.
• « ∞ » is slowest but most convenient.

Resources:

• the « $SBOO_EMACS_DESKTOP_RESTORE_EAGER » environment variable.

Related:

• `desktop-restore-eager'"

  (let* ((VALUE-DYNAMIC (condition-case nil
                            (sboo-getenv-number "SBOO_EMACS_DESKTOP_RESTORE_EAGER")
                          (error nil)))
         (VALUE-STATIC  500)
         )

    (or (and (integerp VALUE-DYNAMIC) (<= 0 VALUE-DYNAMIC) VALUE-DYNAMIC)
        VALUE-STATIC)))

;; M-: (progn (setenv "SBOO_EMACS_DESKTOP_RESTORE_EAGER" "10") (sboo-desktop-restore-eager))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-no-desktop-read-at-startup nil

      "Set this variable to a non-nil value if you do not want to enable
`desktop-save-mode'.

This variable can be used to start emacs without reading the previously
saved desktop at startup:

> emacs --eval \"(setq sboo-no-desktop-read-at-startup t)\"

(NOTE: `defvar' won't overwrite a prior `setq', so this works.)")

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

(defcustom sboo-desktop-globals-to-save

  '(
    (comint-input-ring        . 50)
    desktop-missing-file-warning
    (dired-regexp-history     . 20)
    (extended-command-history . 30)
    (face-name-history        . 20)
    (file-name-history        . 100)
    (ido-buffer-history       . 100)
    (ido-last-directory-list  . 100)
    (ido-work-directory-list  . 100)
    (ido-work-file-list       . 100)
    (magit-read-rev-history   . 50)
    (minibuffer-history       . 50)
    (org-refile-history       . 50)
    (org-tags-history         . 50)
    (query-replace-history    . 60)
    (read-expression-history  . 60)
    (regexp-history           . 60)
    (regexp-search-ring       . 20)
    register-alist
    (search-ring              . 20)
    (shell-command-history    . 50)
    ;; tags-file-name
    ;; tags-table-list
    )

  "Extra `desktop-globals-to-save'.

See URL `https://github.com/kaushalmodi/.emacs.d/blob/08f8256f3de346bf6d389f922c52b4605f700fc4/setup-files/setup-desktop.el#L55'."

  :type  '(repeat (restricted-sexp :match-alternatives (symbolp consp)))
  :safe  t
  :group 'sboo-desktop)

;;----------------------------------------------;;

(defcustom sboo-desktop-files-not-to-save

  (let ((default-desktop-files-not-to-save
          (eval
           (car (get 'desktop-files-not-to-save
                     'standard-value))))
        )

      (setq desktop-files-not-to-save

            (eval
             `(rx (or (regexp ,default-desktop-files-not-to-save)
                      (and (or ".desktop"
                               ;; Don't save .gpg files. Restoring those files
                               ;; in emacsclient causes a problem as the
                               ;; password prompt appears before the frame is
                               ;; loaded.
                               ".gpg"
                               ;; If backup files with names like
                               ;; "file.sv.1.bkp" are saved to the desktop file,
                               ;; emacsclient crashes at launch.
                               ".bkp"
                               ;; don't re-open emacs builtin files (jumped to from `describe-*').
                               ".el.gz"
                               ;;
                               "TAGS")
                           line-end))))))

  "Extra `desktop-files-not-to-save'.

See URL `https://github.com/kaushalmodi/.emacs.d/blob/08f8256f3de346bf6d389f922c52b4605f700fc4/setup-files/setup-desktop.el#L81'."

  :type  '(choice (const :tag "None" nil)
                  regexp)
  :safe  t
  :group 'sboo-desktop)

;;----------------------------------------------;;
;;; Functions ----------------------------------;;
;;----------------------------------------------;;
;;; `:init'      (sboo-xdg-data "" :subdir "emacs/desktop")

(defun sboo-desktop-init! ()

  "Initialize `desktop-mode' variables."
  (interactive)

  (setq desktop-dirname sboo-desktop-directory)

  ;; ^

  (setq desktop-restore-eager (sboo-desktop-restore-eager)) ;TODO prioritize a few core buffers like home.nix and emacs.md

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

  (setq desktop-globals-to-save
        (append sboo-desktop-globals-to-save desktop-globals-to-save))

  (setq desktop-files-not-to-save
        sboo-desktop-files-not-to-save)

  (make-directory (file-name-directory sboo-desktop-directory)
                  :create-parent-directories)

  (add-hook 'desktop-after-read-hook #'sboo-bury-all-star-buffers)
  ;(add-hook 'desktop-delay-hook      #'sboo-bury-all-star-buffers)

  (when (and sboo-desktop-enable (null sboo-no-desktop-read-at-startup))
    (desktop-read)
    (desktop-save-mode +1))

  ;; ^ Enable globally.

  ())

;TODO `desktop-save' on emacs exit (i.e. `C-x C-c`).

;; Notes:
;;
;; (desktop-save DIRNAME :release :only-if-changed "...")
;;
;; e.g. « (desktop-save sboo-desktop-directory) »
;;

;TODO desktop-save-in-desktop-dir

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

;;; `desktop-files-not-to-save'
;;
;; M-: (eval (car (get 'desktop-files-not-to-save 'standard-value)))
;;   ⇒ "\\(^/[^/:]*:\\|(ftp)$\\)"
;;
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