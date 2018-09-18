;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `org-mode' configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org

  :commands (org-mode)

  :bind (:map org-mode-map
              ("TAB"   . dabbrev-expand)
              ("<tab>" . dabbrev-expand)
              )

  :config
  (progn
    (setq
     org-tags-match-list-sublevels                  nil
     org-complete-tags-always-offer-all-agenda-tags t
     org-agenda-files                               '("~/Dropbox/issues.org"))))

;; ^ 
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; org
;; 
;; 

;; See:
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-org)