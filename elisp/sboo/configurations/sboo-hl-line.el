;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; `hl-line` Highlighting the Current Line

(defun sboo-config-hl-line ()
  ""
  (interactive)

  (global-hl-line-mode 1)
  ;; ^ enable hl-line.
  ;; i.e. continuously highlight the current line of the active window (of the topmost frame?)

  (set-face-background 'hl-line "#dedede")
  ;; ^ Set a color as the background face of the current line.
  ;; "#dedede" is Light-Gray.

  (set-face-foreground 'highlight nil)
  ;; ^ keep syntax highlighting in the current line.

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-hl-line)