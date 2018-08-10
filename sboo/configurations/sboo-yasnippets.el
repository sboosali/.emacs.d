;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet: text expansion.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet

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
  ;;

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