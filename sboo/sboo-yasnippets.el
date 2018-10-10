;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-yas-reload ()

  "Recompile and reload all `.yasnippet' files."
  (interactive)

  (progn
    (yas-recompile-all)
    (yas-reload-all)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `yas-next-field-or-maybe-expand':
;; 
;; If a `key' is before `point', try to expand a snippet (`yas-expand').
;; Otherwise, delegate to `yas-next-field'.
;;

;; `yas-reload-all':
;;
;;     (yas-reload-all &optional NO-JIT)
;;
;; When `NO-JIT' is non-`nil', force immediate reload of all known snippets under ‘yas-snippet-dirs’.
;; Otherwise, use just-in-time loading.
;;

;; `yas-snippet-dirs':
;;
;; Each item in this list is a top-level directory,
;; which holds per-mode snippet directories.
;;
;; e.g.
;;
;;     $ find ~/.emacs.d/sboo/snippets/ -type f -name '*.yasnippet'
;;     ~/.emacs.d/sboo/snippets/haskell-mode/*.yasnippet
;;     ~/.emacs.d/sboo/snippets/emacs-lisp-mode/*.yasnippet
;;     ~/.emacs.d/sboo/snippets/...
;;

;; See:
;;
;; - http://joaotavora.github.io/yasnippet/
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-yasnippets)