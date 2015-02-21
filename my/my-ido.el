(provide 'my-ido)
(require 'ido)
(require 'ido-complete-space-or-hyphen)

; http://www.masteringemacs.org/article/introduction-to-ido-mode


(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

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

(setq confirm-nonexistent-file-or-buffer nil)

;; (setq ido-file-extensions-order
;;  '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))

; enable IDO to use completion-ignored-extensions
(setq ido-ignore-extensions t)

(add-to-list 'completion-ignored-extensions ".DS_Store")
(add-to-list 'completion-ignored-extensions "#")
(add-to-list 'completion-ignored-extensions "Icon")

