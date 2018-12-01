;; -*- lexical-binding: nil; -*-

(defun my-annotation-function (s)
  (message "[my-annotation-function] %S" minibuffer-completion-table)
  (let ((item
         (when (consp minibuffer-completion-table)
           (assoc s minibuffer-completion-table))))
    (when item
      (let ((line (concat "  -- " (second item))))
        (message "[my-annotation-function] %s" line)
        line))))

(defvar my-completions
  '(("a" . "description of a")
    ("b" . "b's description")))

;; (let ((completion-extra-properties
;;         (list :annotation-function #'my-annotation-function)))
;;   (message
;;    (completing-read "Prompt: " my-completions)))

(progn
  (setq completion-extra-properties
        (list :annotation-function #'my-annotation-function))
  (message
    (completing-read "Prompt: " my-completions)))

