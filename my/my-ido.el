(provide 'my-ido)

(require 'ido)
;(require 'ido-complete-space-or-hyphen)
(require 'dash)
(require 's)

; http://www.masteringemacs.org/article/introduction-to-ido-mode



(setq ido-use-filename-at-point 'guess)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-default-file-method 'other-window)
(setq ido-ignore-buffers '(
 "^ "
 "*Completions*"
 "*Shell Command Output*"
 "*Messages*"
 "Async Shell Command"
 "*scratch*"
 "*Messages*"
 "*Quail Completions*"
))

;;; http://stackoverflow.com/questions/2373333/how-can-i-get-emacs-to-open-a-new-buffer-quickly
(setq confirm-nonexistent-file-or-buffer 'after-completion)

;; (setq ido-file-extensions-order
;;  '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))

; enable IDO to use completion-ignored-extensions
(setq ido-ignore-extensions t)
(--each '(".DS_Store" "#" "Icon" "testingtesting")
 (add-to-list 'completion-ignored-extensions it))

;; (defun my/ido-stars-to-end ()
;;   "Put \"*starred*\" buffers at the end of the ido candidates list."
;;   (ido-to-end (--filter (and (s-starts-with-p "*" it) (s-ends-with-p "*" it))
;;                         ido-temp-list)))
;; (add-hook 'ido-make-buffer-list-hook 'my/ido-stars-to-end)

(ido-mode 1)
