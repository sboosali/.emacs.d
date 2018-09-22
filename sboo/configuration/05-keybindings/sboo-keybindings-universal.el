;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-bind-standard-keys! ()
  
  "Reconfigure emacs to use the standard keyboard-shortcuts for: {Open, Close, Save, Save As, Select All, …}.
  "
  (interactive)

  (progn
    
    ;; make {copy, cut, paste, undo} have {C-c, C-x, C-v, C-z} keys
    (cua-mode 1)

    ;; Select All. was move-beginning-of-line
    (global-set-key (kbd "C-a") 'mark-whole-buffer-buffer)

    ;; Find. was forward-char
    (global-set-key (kbd "C-f") 'isearch-forward)

    ;; New. was next-line
    (global-set-key (kbd "C-n") 'xah-new-empty-buffer)

    ;; New Window. was nil
    (global-set-key (kbd "C-S-n") 'make-frame-command)

    ;; Open. was open-line
    (global-set-key (kbd "C-o") 'ido-find-file)

    ;; Save. was isearch-forward
    (global-set-key (kbd "C-s") 'save-buffer)

    ;; Save As. was nil
    (global-set-key (kbd "C-S-s") 'write-file)

    ;; Paste. was scroll-up-command
    (global-set-key (kbd "C-v") 'yank)

    ;; Close. was kill-region
    (global-set-key (kbd "C-w") 'kill-buffer)

    ;; Redo. was yank
    (global-set-key (kbd "C-y") 'redo)

    ;; Undo. was suspend-frame
    (global-set-key (kbd "C-z") 'undo)
    ;;

    t))

;; ^ NOTE
;; the keybindings abovem besides `cua-mode', don't work well;
;; because random major-modes or minor-modes will override those keys,
;; and because you lose some default emacs cursor movement keys
;; (e.g. `C-f' in `isearch', to move within your query,
;; since the `<right>' arrow (by default) moves within the buffer being searched,
;; not the "current" minibuffer you're entering/editing the search query in).
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; 
;; 

;; See:
;;     - http://ergoemacs.org/emacs/emacs_make_modern.html
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-keybindings-universal)