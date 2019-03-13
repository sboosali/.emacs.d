;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Configure `flycheck'.
;;
;; 
;;
;; 

;;; Code:

;;==============================================;;
;;; Utilities ==================================;;
;;==============================================;;

;;==============================================;;
;;; Variables ==================================;;
;;==============================================;;

(defvar sboo-flycheck-display-buffer

  `( ,(rx bos "*Flycheck errors*" eos)
     (display-buffer-reuse-window display-buffer-in-side-window)
     (side            . bottom)
     (reusable-frames . visible)
     (window-height   . 0.33)
   )

  "This display rule tells Emacs to always display the error list at the bottom side of the frame, occupying a third of the entire height of the frame.

Links:

• URL `http://www.flycheck.org/en/latest/user/error-list.html#tune-error-list-display'.")

;;==============================================;;
;;; Commands ===================================;;
;;==============================================;;

(defun sboo-flycheck ()

  "Show « Flycheck Errors ».

Calls `flycheck-list-errors'.

« Flycheck Errors » has major mode `flycheck-error-list-mode'.

With `sboo-flycheck-display-buffer' in `display-buffer-alist', 
the `flycheck-error-list-mode' buffer is shown on the bottom window.

Links:

• URL `'."

  (interactive)

  (delete-other-windows)
  (flycheck-list-errors))

;; ^ NOTE `flycheck-list-errors' calls `display-buffer'.

;;==============================================;;
(provide 'sboo-flycheck)