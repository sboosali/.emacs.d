(provide 'my-register)

(setq register-separator ?+)
(set-register register-separator "\n")

(defun my-register-append () (interactive)
 (append-to-register 9 (region-beginning) (region-end))) 

(defun my-register-insert () (interactive)
 (insert-register 9)) 

(defun my-register-clear () (interactive)
 (copy-to-register 9 1 1)) 

(global-set-key (kbd "C-x C-y r c") 'my-register-append) 
(global-set-key (kbd "C-x C-y r v") 'my-register-insert) 
(global-set-key (kbd "C-x C-y r d") 'my-register-clear) 


