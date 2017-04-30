(provide 'my-org)
(require 'org)

; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html

(setq org-startup-truncated nil)

(setq org-log-done t)

(setq org-startup-folded 'nofold) ; #+STARTUP: nofold

(setq org-agenda-files (list
 "~/diary/home.org"
 "~/diary/work.org"
 "~/diary/chores.org"
))

; describe-function
;  org-agenda
