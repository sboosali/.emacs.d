;;; sboo-os-window.el --- -*- lexical-binding: t -*-

;;----------------------------------------------;;

;; (use-package window

;;   :custom (switch-to-buffer-obey-display-actions t)

;;   :bind
;;   (("C-x s" . #'window-toggle-side-windows)
;;    ("C-x q" . #'bury-buffer)
;;    ("C-x Q" . #'unbury-buffer))

;;   :config
;;   ()
;;   )

;; ^ (manually) ‘switch-to-buffer’ing must obey ‘buffer-display-action-alist’, like (programmatically) ‘display-buffer’ing obeys it.

;; (add-to-list 'display-buffer-alist
;;   '("\\*Help\\*"
;;      (display-buffer-reuse-window display-buffer-pop-up-window)))

;; (defconst sboo-prog-mode/display-buffer-alist
;;   (let ((WIN-PARAMS
;;           '(window-parameters . ((no-other-window         . t)
;;                                  (no-delete-other-windows . t)))))

;;     `((,(rx "*" (or "Buffer List") "*")
;;         display-buffer-in-side-window
;;         (side . top)
;;         (slot . 0)
;;         (window-height . ,#'fit-window-to-buffer)
;;         (preserve-size . (nil . t))  ; keep ‘window-height’ same / don't vertically-resize.
;;         ,WIN-PARAMS)

;;       (,(rx "*" (or "xref" "Tags List") "*")  ; = "\\*\\(?:xref\\|Tags List\\)\\*"
;;         display-buffer-in-side-window
;;         (side . right)
;;         (slot . 0)     ; in-the-middle-of.
;;         (window-width . ,#'fit-window-to-buffer)
;;         (preserve-size . (t . nil))  ; keep ‘window-width’ the same / don't horizontally-resize.
;;         ,WIN-PARAMS)

;;       (,(rx "*" (or "dired") "*")
;;         display-buffer-in-side-window
;;         (side . left)
;;         (slot . 0)
;;         (window-width . ,#'fit-window-to-buffer)
;;         (preserve-size . (t . nil))
;;         ,WIN-PARAMS)

;;       (,(rx "*" (or "Completions" "help" "Info" "grep") "*")
;;         display-buffer-in-side-window
;;         (side . bottom)
;;         (slot . -1)  ; above-or-left-of.
;;         (preserve-size . (nil . t))
;;         ,WIN-PARAMS)

;;       (,(rx "*" (or "shell" "eshell" "term" "vc" "compilation") "*")
;;         display-buffer-in-side-window
;;         (side . bottom)
;;         (slot . +1)  ; below-or-right-of.
;;         (preserve-size . (nil . t))
;;         ,WIN-PARAMS)

;;        (,(rx (or (bol "test" (char ?_ ?-))
;;                  ((or "Test" "Tests") eol)))
;;         display-buffer-in-direction
;;         (direction . right)))

;;     )
;;   "A ‘display-buffer-alist’ for “IDE Panels”:

;; • on the right, an XRef/TAGS buffer;
;; • on the left, a DirEd/¿Project? buffer;
;; • at the bottom-left, a Completions/Help/Info/Grep buffer;
;; • at the bottom-right, a Shell/Term/Compilation/VC buffer;
;; • at the top, the Buffer-List buffer.")
;; ;;
;; ;;  ___________________________________
;; ;; |    *Buffer List*                  |
;; ;; |___________________________________|
;; ;; |     |                       |     |
;; ;; |  *  |                       |  *  |
;; ;; |  d  |                       |  T  |
;; ;; |  i  |                       |  a  |
;; ;; |  r  |   Main Window Area    |  g  |
;; ;; |  e  |                       |  s  |
;; ;; |  d  |                       |  *  |
;; ;; |  *  |                       |     |
;; ;; |_____|_______________________|_____|
;; ;; | *help*/*grep*/  |  *shell*/       |
;; ;; | *Completions*   |  *compilation*  |
;; ;; |_________________|_________________|
;; ;; |             Echo Area             |
;; ;; |___________________________________|
;; ;; 

;; (defun sboo-prog-mode-ui (&optional disable-p)
;;   "Enable “IDE Panels”."
;;   (interactive "P")

;;   (if disable-p
;;       (prog
;;         (setq window-sides-slots (list nil nil nil nil))
;;         (setq display-buffer-alist nil))

;;     (setq fit-window-to-buffer-horizontally t)
;;     (setq window-resize-pixelwise           t)
;;     (let ((LEFT   1)
;;           (TOP    2)
;;           (RIGHT  1)
;;           (BOTTOM 2))
;;       (setq window-sides-slots (list LEFT TOP RIGHT BOTTOM))
;;     (setq display-buffer-alist sboo-prog-mode/display-buffer-alist)))

;;

;;----------------------------------------------;;

(provide 'sboo-window)