(provide 'my-python)
(require 'smart-tabs-mode)


(defun my-python-mode-hook ()
  (smart-tabs-advice python-indent-line-1 python-indent)

  (setq tab-width 4)
  (setq py-indent-offset 4)

  (setq py-indent-offset tab-width)
  (setq python-indent tab-width)
  (setq py-smart-indentation nil)

  (setq-default show-trailing-whitespace t)
  ;(add-hook 'before-save-hook 'delete-trailing-whitespace)

  (setq py-indent-comments nil)
  (setq py-electric-comment-p nil)
)

;;; How to only enable for BUffers under Python mode
;(add-hook 'python-mode-hook 'my-python-mode-hook)

