(provide 'my-linum)
(require 'linum)

(global-linum-mode 1)

; relative line numbers, modulo a hundred 
(setq linum-format (lambda(n) (number-to-string (mod n 100))))

; (defun linum-update-window (win)
