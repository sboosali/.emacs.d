;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet: text expansion.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet

  :demand t

  :mode
  ("\\.yasnippet\\'" . snippet-mode)

  :bind
  (("<kp-home>" . yas-next-field-or-maybe-expand)
   )
  
  :init
  (setq yas-snippet-dirs
        (list (sboo-emacs-file "sboo/snippets")))
  ;; ^ each item in this list is:
  ;; a top-level directory, which holds per-mode snippet directories.
  ;;
  ;; e.g.:
  ;;     $ find ~/.emacs.d/sboo/snippets/ -type f -name '*.yasnippet'
  ;;     ~/.emacs.d/sboo/snippets/haskell-mode/*.yasnippet
  ;;     ~/.emacs.d/sboo/snippets/emacs-lisp-mode/*.yasnippet
  ;;     ~/.emacs.d/sboo/snippets/...
  ;;
  ;; NOTE use `setq', not `add-to-list', to remove the default snippets-directory.
  ;;

  (add-hook 'snippet-mode-hook
            (lambda () (real-auto-save-mode -1)))
  ;; ^ HACK: works-around `snippet-mode''s obnoxious behavior
  ;; of flinging the cursor somewhere else in the buffer, on each save.

  :config
  (yas-reload-all t)
  ;; ^
  ;;
  ;; `yas-reload-all':
  ;;
  ;;     (yas-reload-all &optional NO-JIT)
  ;;
  ;; When NO-JIT is non-`nil', force immediate reload of all known
  ;; snippets under ‘yas-snippet-dirs’, otherwise use just-in-time loading.
  ;;
  (yas-global-mode 1))

 ;; ^ 
 ;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-yas-reload ()
  "Recompile and reload all `.yasnippet's."
  (interactive)
  (progn
    (yas-recompile-all)
    (yas-reload-all)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; `yas-next-field-or-maybe-expand':
;; ---
;; If a `key' is before `point', try to expand a snippet (`yas-expand').
;; Otherwise, delegate to `yas-next-field'.

;; See
;;     - http://joaotavora.github.io/yasnippet/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-yasnippets)