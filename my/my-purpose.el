(provide 'my-purpose) 
(require 'window-purpose)
(require 'window-purpose-x)
;;; https://github.com/bmag/emacs-purpose

(purpose-x-kill-setup)

(purpose-x-magit-multi-on)

;; [general] is the default purpose  
(setq purpose-use-default-configuration nil)
(add-to-list 'purpose-user-mode-purposes '(prog-mode . code))
(add-to-list 'purpose-user-mode-purposes '(haskell-mode . code))
(add-to-list 'purpose-user-mode-purposes '(shell-mode . other))
(add-to-list 'purpose-user-name-purposes '("notes" . other))
(purpose-compile-user-configuration)

(purpose-mode)

