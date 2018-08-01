;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'sboo-variables)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-init-effects()

  "Miscellaneous actions to be executed during initialization,
  before executing the rest of my settings.
  Among other effects, a few files are opened here, to ensure they're immediately available, even if my session-management isn't working (for example, if my `desktop-mode` settings are bad, or the `.desktop` file was deleted, or so on)."
  (interactive)

  (find-file user-init-file)
  ;; ^ Opens the init file itself,
  ;; e.g. "~/.emacs.d/init.el".
  ;; For conveniently iterating on and/or debugging
  ;; the `init.el` itself.
  
  (find-file "~/haskell/")
  ;; ^ All my haskell-language projects.

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-init-effects)