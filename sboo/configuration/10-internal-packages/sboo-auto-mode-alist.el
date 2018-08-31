;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `auto-mode-alist' maps filepaths to major-modes.
;;
;; in particular:
;;
;; - file extensions (e.g. `.hs');
;; - basenames (e.g. `README.md')..
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-add-auto-mode-file-extension (FileExtension MajorMode &optional Pure)
  "Associates the `FileExtension' with the `MajorMode' in `auto-mode-alist'.

  If you set `Pure', this function is pure, returning the association (a cons cell),
  without mutating the global variable `auto-mode-alist'.

  e.g. 
      (sboo-add-auto-mode-file-extension \"hs\" 'haskell-mode)
      (\"\\.hs\\'\" . 'haskell-mode)
  "

  (let* ((Pattern
          (concat "\\."
                  FileExtension
                  "\\'"))

         (Association
          `(,Pattern . ,MajorMode)))

    (progn

      (unless Pure
        (progn
          (add-to-list 'auto-mode-alist Association)
          (message "%S" Association)))

      Association)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-add-auto-mode-basename (BaseName MajorMode &optional Pure)
  "Associates the file `BaseName' with the `MajorMode' in `auto-mode-alist'.

  If you set `Pure', this function is pure, returning the association (a cons cell),
  without mutating the global variable `auto-mode-alist'.

  e.g. 
      M-: (sboo-add-auto-mode-basename \"README.md\" 'gfm-mode t)
          (\"README\\.md\\'\" . 'gfm-mode)
  "

  (let* ((Pattern
          (concat (replace-regexp-in-string "\\." "\\\\." BaseName)
                  "\\'"))

         (Association
          `(,Pattern . ,MajorMode)))

    (progn

      (unless Pure
        (progn
          (add-to-list 'auto-mode-alist Association)
          (message "%S" Association)))

      Association)))

;; ^ internals:
;;
;; M-: (replace-regexp-in-string "\\." "\\\\." "README.md")
;;     ("\\.hs\\'" . 'haskell-mode)
;;
;; M-: (message "%S" '("x" . y))"
;;     (\"x\" . y)"
;;
;; M-: (message "%s" '("x" . y))
;;     (x . y)"
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (sboo-add-auto-mode-file-extension "knsrc"   'conf-mode)
  ;; ^ `.knsrc' files have the `INI' format, which `conf-mode' supports.

  (sboo-add-auto-mode-basename       "LICENSE" 'text-mode)
  (sboo-add-auto-mode-basename       "TODO"    'text-mode)
  (sboo-add-auto-mode-basename       "NOTES"   'text-mode)

  ;;TODO any file that ends in `rc`, should we default to 'conf-mode or to 'sh-mode?
  ;;;(add-to-list 'auto-mode-alist ("rc\\'" . 'conf-mode))
  ;;;(add-to-list 'auto-mode-alist ("rc\\'" . 'sh-mode))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; `auto-mode-alist':
;;
;; e.g. 
;;     (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;
;; The auto-mode-alist variable is an AssociationList that associates MajorModes with a pattern to match a buffer filename when it is first opened.
;;

;; NOTE
;; 
;; A \' matches the end of a string, whereas $ matches the empty string before a newline. Thus, $ may lead to unexpected behavior when dealing with filenames containing newlines (admittedly uncommon).
;; 
;; The \. matches ‘.’ (a period); ‘.’ must be escaped by a backslash because the period is a special character in Regular Expressions.
;; 
;;

;; See:
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-auto-mode-alist)