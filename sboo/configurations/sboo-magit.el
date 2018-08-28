;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :disabled t
  :if       (executable-find "git")
  :defer    t
  :commands magit-status
  :init
  (setq
   magit-auto-revert-mode nil))
   ;; ^
   ;; automatic reversion, seemingly, misbehaves with `real-auto-save`:
   ;; the file of a file-buffer is silently modified to be the backup.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-magit)