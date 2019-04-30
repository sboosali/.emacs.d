;;; -*- lexical-binding: t -*-

;;==============================================;;
;;; Commentary:

;; Personal XModMap configuration.
;;
;; Commands:
;;
;; • `sboo-xmodmap-set-compile-command'
;;
;; Variables:
;;
;; • `sboo-xmodmap-hooks-list'
;; • `sboo-xmodmap-program'
;;

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;;; Requirements -------------------------------;;
;;----------------------------------------------;;

(eval-when-compile

  (require 'cl-lib))

;;----------------------------------------------;;

(progn

  (require 'subr-x))

;;----------------------------------------------;;
;; Customization -------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-xmodmap

  nil

  "Personal Xmodmap customization."

  :prefix "sboo-xmodmap-"

  :group 'sboo
  :group 'xmodmap)

;;==============================================;;

(defcustom sboo-xmodmap-hooks-list

  (list #'sboo-xmodmap-set-compile-command
        #'sboo-set-font-to-iosevka
        )

  "Hooks for `xmodmap-mode'.

Zero-or-more function-symbols."

  :type '(repeat (function))

  :safe t
  :group 'sboo-xmodmap)

;;; sub word mode lets you navigate (e.g. M-b) between "sub words" of a camelcased word

;;----------------------------------------------;;

(defcustom sboo-xmodmap-program

  "xmodmap"

  "Which program `xmodmap' to run on « .xmodmap »-formatted files.

a `stringp'."

  :type '(string :tag "Program")

  :safe #'stringp
  :group 'sboo-xmodmap)

;;----------------------------------------------;;
;; Functions: `compile' ------------------------;;
;;----------------------------------------------;;
 
(cl-defun sboo-xmodmap-set-compile-command (&key buffer)

  "Set `compile-command' to `sboo-xmodmap-get-compile-command'.

See:

• `sboo-xmodmap-set-compile-command'"

  (let* ((COMPILE-COMMAND (sboo-xmodmap-get-compile-command :buffer buffer))
         )

    (setq-local compile-command COMPILE-COMMAND)

    ()))

;;----------------------------------------------;;

(cl-defun sboo-xmodmap-get-compile-command (&key buffer)

  "Return a `compile-command' for an Xmodmap file.

See:

• `sboo-xmodmap-program'"

  (let* ((FILE (buffer-file-name buffer))

         (COMPILE-COMMAND (string-join (list sboo-xmodmap-program FILE "") " "))
         )

    COMPILE-COMMAND))

;;----------------------------------------------;;

;;TODO `compilation-error-regexp-alist-alist'...
;;
;; e.g. program `xmodmap':
;;
;; $ xmodmap /home/sboo/configuration/xmodmap/sboo.xmodmap 
;;   xmodmap:  unknown command on line /home/sboo/configuration/xmodmap/sboo.xmodmap:69
;;   xmodmap:  1 error encountered, aborting.
;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;;
;; 
;;
;;==============================================;;
(provide 'sboo-xmodmap)