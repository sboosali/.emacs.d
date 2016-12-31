(provide 'my-windmove)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-S-<left>") 'windmove-left)
(global-set-key (kbd "C-S-<right>") 'windmove-right)
