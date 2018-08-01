;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HISTORY
;; the builtin `savehist` library.

(require 'savehist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (setq savehist-additional-variables
        '(search-ring
          regexp-search-ring
          compile-history
          kill-ring))
  ;; ^ 
  ;; By default, Savehist mode saves only your minibuffer histories.
  ;; The line above saves your search strings, etc, too.
  ;;
  ;; All minibuffer histories are saved automatically,
  ;; so you don’t need to add minibuffer history variables to the
  ;; `savehist-additional-variables` list.
  ;; 

  (setq savehist-file
        (concat sboo-emacs-directory
                "persisted/savehist/savehist.el"))

  (savehist-mode 1)
  ;; ^ enabling `savehist-mode` must come after all `savehist-*` customizations
  ;; (otherwise, they will be ignored).
)
;; ^
;; NOTE, Emacs truncates minibuffer history automatically,
;; so the file shouldn't grow indefinitely.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES
;;
;; `savehist` Persists arbitrary `read`able `elisp` variables.
;;
;; Values which can't be read back can't be saved and restored, but anything else could be persisted.
;; savehist doesn't care whether or not a variable "changes regularly", but if the variable doesn't contain a history of values then savehist isn't necessarily very useful -- savehist doesn't track the sequence of values of a variable over time; it just saves the current value of the variables it's interested in. Obviously that's fine for variables containing a list of historical values. For variables with a single changing value, you would just be remembering the most recent one of those (and any changes to that value which occurred between savehist saves wouldn't be noticed at all).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-savehist)