;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `isearch'
;;
;; a.k.a. "Interactive SEARCH".
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; this `feature' provides:
;;
;; - arrow-keys for keybindings of `isearch-mode-map'
;; - `xah-search-current-word'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-utilities-internal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xah-search-current-word ()

  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)

  (let ( $p1 $p2 )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq $p1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq $p2 (point))))

    (setq mark-active nil)

    (when (< $p1 (point))
      (goto-char $p1))

    (isearch-mode t)

    (isearch-yank-string (buffer-substring-no-properties $p1 $p2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (sboo-bind-key-globally! '("<f2>" . xah-search-current-word))

  (sboo-bind-keys-for! isearch-mode-map
                       '(("<up>"    . isearch-ring-retreat)
                         ("<down>"  . isearch-ring-advance)
                         ("<left>"  . isearch-repeat-backward)
                         ("<right>" . isearch-repeat-forward))))

  ;; ^ provides these (single-keypress) keybindings:
  ;; .
  ;; → 
  ;; next occurrence
  ;; .
  ;; ← 
  ;; previous occurrence
  ;; .
  ;; ↑
  ;; previous search term
  ;; .
  ;; ↓
  ;; next search term
  ;; .
  ;; Enter
  ;; exit isearch
  ;; .

;; ^ 
;;TODO minibuffer-local-isearch-map (kbd "<f2>") #'isearch-repeat-forward)
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; `isearch' has its own bindings that are active once you start an incremental search.

;; NOTE Emacs 24.4 has a new command `isearch-forward-symbol-at-point' but has these problems:
;;
;; - isearch-forward-symbol-at-point is on “symbols” only. 
;; What chars are part of “symbol” is unpredictable, mode-dependent.
;;
;; - The search is with boundary check.
;; That is, if current symbol is “xyz”, it'll will not find occurrences of “xyz2”.
;;
;;

;; See:
;;     - http://ergoemacs.org/emacs/emacs_isearch_by_arrow_keys.html
;;     - http://ergoemacs.org/emacs/emacs_isearch_by_arrow_keys.html#google_vignette
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-isearch)