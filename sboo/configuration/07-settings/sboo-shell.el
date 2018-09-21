;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage multiple `shell's.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-shell-emacs-buffer-name "*emacs*" "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-launch-shell-emacs ()

 "Wraps `shell'.
 "
 (interactive)

 (let ((default-directory (or sboo-emacs-directory "~/.emacs.d/")))
   (shell sboo-shell-emacs-buffer-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; TODO
;;
;; > If a file ‘~/.emacs_SHELLNAME’ exists, or ‘~/.emacs.d/init_SHELLNAME.sh’, it is given as initial input.
;; 

;; See:
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-shell)