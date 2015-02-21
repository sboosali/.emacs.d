(provide 'my-commands)


(defvar last-post-command-position 0
  "Holds the cursor position from the last run of post-command-hooks.")

(make-variable-buffer-local 'last-post-command-position)

(defun echo-if-moved-post-command ()
  (let ((current-word (thing-at-point 'word)))
    (message "%s" current-word))
  (setq last-post-command-position (point)))

(add-to-list 'post-command-hook #'echo-if-moved-post-command)
