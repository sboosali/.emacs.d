;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-prefix-command 'sboo-keymap)

;; ^ 


(kbd "<s>")

;; ^ 

(global-set-key 'sboo-keymap)

;; ^ 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-bind-keys! (BINDINGS &optional KEYMAP)

  ""

  (dolist (Binding BINDINGS)
    (pcase Binding

      ((KeySequence . Command)

        (define-key KEYMAP (kbd KeySequence) Command))

      (_

        (message "Ignoring « %S », which isn't a keybinding (i.e. a keysequence-dot-command cons-cell)." Binding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (let ((bindings '(("-" . split-window-below)
;;                   ("|" . split-window-right)
;;                   ("d" . delete-window)
;;                   ("n" . other-window)
;;                   ("h" . windmove-left)
;;                   ("j" . windmove-down)
;;                   ("k" . windmove-up)
;;                   ("l" . windmove-right))))

(let ((bindings '(("-" . split-window-below)

                  ("i t" . image-toggle-display)

                  ("p g" . #'projectile-grep)
                  ("p f" . #'projectile-find-file)
                  ("p c" . #'projectile-compile-project)

                  )))

 (sboo-bind-keys! bindings sboo-keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See:
;;
;; - 
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-keymap)