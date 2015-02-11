(provide 'my-ido)
(require 'ido)
(require 'ido-complete-space-or-hyphen)

(ido-mode t)

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

