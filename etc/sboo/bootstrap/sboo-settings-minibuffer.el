;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration of various MiniBuffers.
;;
;; Common MiniBuffers include:
;;
;; - `minibuffer-inactive-mode': the search prompt.
;; - 
;; - 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-minibuffer-config () ;;TODO rename to sboo-init-... ?

  (interactive)

  (progn

    (define-key minibuffer-inactive-mode-map
      (kbd "<tab>") 'dabbrev-expand)

    (define-key minibuffer-inactive-mode-map
      (kbd "TAB") 'dabbrev-expand)

    ()))

 ;; ^ globally, `TAB' is bound to `dabbrev-expand', which works in most modes;
 ;; but locally, we must bind the "earlier" `<tab>' too (for some reason).
 ;;
 ;; TODO-doesnt-work

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Discovery
;; =========
;; 
;; [1] open a mini-buffer (e.g. `C-s` for the search mini-buffer, a.k.a `minibuffer-inactive-mode');
;; [2] then, focused on the minibuffer, run `describe-mode` (i.e. `C-h m`).
;;

;; See:
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-settings-minibuffer)