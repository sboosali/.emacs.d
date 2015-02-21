(provide 'my-speedbar)

(setq speedbar-use-images nil)

; shrink font
(make-face 'speedbar-face)
(set-face-font 'speedbar-face "Inconsolata-12")
(setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))

; exclude patterns to allow speedbar-expand-all to be blindly
; recursive, because I couldn't get a predicate in the while loop to
; work.
; this doesn't work either
;; (speedbar-add-ignored-directory-regexp ".*ignore$")
;; (speedbar-add-ignored-directory-regexp ".*dist$")

; http://www.emacswiki.org/emacs/SpeedBar
; later, should parse .cabal, or at least extract hs-source-dirs
; or have a white list of files/directories, for example the Makefile
; good to expand.
;; (defun speedbar-expand-most ()
;;  "Expand most items under the current selection in the speedbar buffer."
;;  (interactive)
;;  (goto-char (point-min))
;;  (forward-line) ; skip the already-expanded current directory
;;  (while (not (eobp))
;;   (if (and (not (string/ends-with (current-line) "dist"))
;;            (not (string/ends-with (current-line) "ignore")))
;;       (speedbar-expand-line))
;;   (forward-line))
;;  (goto-char (point-max))
;; )

(defun speedbar-expand-all ()
 "Expand all items under the current selection in the speedbar buffer."
 (interactive)
 (goto-char (point-min))
 (forward-line) ; skip the already-expanded current directory
 (while (not (eobp))
  (speedbar-expand-line)
  (forward-line))
 (goto-char (point-max))
)

