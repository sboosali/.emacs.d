;;; -*- lexical-binding: t -*-











;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; `yas-insert-snippet':
;;
;; With prefix argument NO-CONDITION, bypass filtering of snippets
;; by condition.
;;

;; `yas-wrap-around-region':
;; 
;; What to insert for snippet’s $0 field.
;; 
;; • If set to a character, insert contents of corresponding register.
;; 
;;   This can be overridden on a per-snippet basis.
;;   A value of ‘cua’ is considered equivalent to ‘?0’ for backwards compatibility.
;; 
;; • If non-nil insert region contents.
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;----------------------------------------------;;


;; http://ergoemacs.org/emacs/emacs_tip_yasnippet_expand_whole_hyphenated_word.html
;; (setq yas-key-syntaxes '("w_" "w_." "^ "))
                                        ; default is (yas-try-key-from-whitespace "w_.()" "w_." "w_" "w") (circa 2019-03-12).


;; # uuid: unique identifier
;; This provides to a way to identify a snippet, independent of its name. Loading a second snippet file with the same uuid would replace the previous snippet.

;; group: snippet menu grouping
;; When expanding/visiting snippets from the menu-bar menu, snippets for a given mode can be grouped into sub-menus . This is useful if one has too many snippets for a mode which will make the menu too long.
;; The # group: property only affect menu construction (See the YASnippet menu) and the same effect can be achieved by grouping snippets into sub-directories and using the .yas-make-groups special file (for this see Organizing Snippets

;;----------------------------------------------;;

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
;; - URL `http://joaotavora.github.io/yasnippet/'
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-yasnippets)