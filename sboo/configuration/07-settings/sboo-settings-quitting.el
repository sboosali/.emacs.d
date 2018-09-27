;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Behavior when Quitting the Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (when (boundp 'confirm-kill-processes)
    (setq confirm-kill-processes nil))

    ;; ^ suppress warnings about open process-buffers (like `*shell*`).
    ;;
    ;; why? because these warnings are spurious, 
    ;; and prevent Emacs from quitting immediately/automatically.

  (setq kill-emacs-query-functions
        (remove 'server-kill-emacs-query-function kill-emacs-query-functions))

        ;; ^ remove `server-kill-emacs-query-function`.
        ;;
        ;; it warns you about:
        ;;
        ;; [1] open process-buffers (like `*shell*`).
        ;; [2] open `emacsclient` buffers.
        ;;

  (add-to-list 'kill-emacs-query-functions #'sboo-desktop-save) ;;TODO this fucks up sometimes (prevents quitting, corrupts desktop, etc)
               
  ())

;; ^ NOTE if `kill-emacs-query-functions` has a buggy hook,
;; run `kill-emacs` directly to exit.
;; the close-button (e.g. the "red 'X'" button at the top-right of a frame) calls
;; `save-buffers-kill-emacs`, which runs the `kill-emacs-query-functions` hooks,
;; before it calls `kill-emacs`.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `kill-emacs-query-functions`:
;;
;;    When save-buffers-kill-terminal is killing Emacs, it calls the functions in this hook, after asking the standard questions and before calling kill-emacs. Calling kill-emacs directly does not run this hook.
;;    The functions are called in order of appearance, with no arguments. Each function can ask for additional confirmation from the user. If any of them returns nil, save-buffers-kill-emacs does not kill Emacs, and does not run the remaining functions in this hook. 
;;
;; See
;;     - https://www.gnu.org/software/emacs/manual/html_node/elisp/Killing-Emacs.html
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-settings-quitting)