;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package evil

  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)

  :config
  (evil-mode 1)

  :bind
  (:map evil-normal-state-map
        (", w" . evil-window-vsplit)))

;; ^ 
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; Underscore "_" Is Not A Word Character.
;; 
;; An underscore "_" is a word character in Vim. This means that word-motions like w skip over underlines in a sequence of letters as if it was a letter itself. In contrast, in Evil the underscore is often a non-word character like operators, e.g. +.
;; 
;; The reason is that Evil uses Emacs' definition of a word and this definition does often not include the underscore. In Emacs word characters are determined by the syntax-class of the buffer. The syntax-class usually depends on the major-mode of this buffer. This has the advantage that the definition of a "word" may be adapted to the particular type of document being edited. Evil uses Emacs' definition and does not simply use Vim's definition in order to be consistent with other Emacs functions. For example, word characters are exactly those characters that are matched by the regular expression character class [:word:].
;; 
;; If you want the underscore to be recognised as word character, you can modify its entry in the syntax-table:
;; 
;; (modify-syntax-entry ?_ "w")
;; This gives the underscore the word syntax-class. You can use a mode-hook to modify the syntax-table in all buffers of some mode, e.g.:
;; 
;; (add-hook 'c-mode-common-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; This gives the underscore the word syntax-class in all C-like buffers
;; 

;; See:
;;     - https://github.com/emacs-evil/evil/blob/master/README.md
;;     - https://github.com/noctuid/evil-guide

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-evil)