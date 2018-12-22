;; -*- lexical-binding: nil; -*-

;;; Commentary:

;; See URL `http://garethrees.org/2015/02/09/emacs/'.

;; $ emacs --no-init-file --find-file ./sboo/examples/original/completion-annotations.el

;;; Code:

(defun my-annotation-function (s)
  (let ((item
	 (assoc s minibuffer-completion-table)))
    (when item
      (concat "  -- " (cdr item)))))

(defvar my-completions
  '(("xxa" . "description of A")
    ("xxb" . "B's description")))

(let ((completion-extra-properties
       '(:annotation-function my-annotation-function)))
  (message
   (completing-read "Prompt: " my-completions)))
