;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MarkDown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode

  :mode
  (("README\\.md\\'" . gfm-mode)
   ;; ^ `gfm-mode', abbreviating "GitHub-flavored markdown".
   ("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))

  :bind (:map markdown-mode-map
              ("TAB" . dabbrev-expand)
         :map gfm-mode-map
              ("TAB" . dabbrev-expand))

  :init
  (setq markdown-command "multimarkdown")

  :commands
  (markdown-mode gfm-mode))

;; ^ 
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes

;; markdown-specific editing features:
;; 
;; - Change heading level two ways.
;; (1) By cycling with:
;;     * ‘C-c C--’ or `M-<left>’
;;     * `C-c C-=’ or `M-<right>’
;; (2) By re-issuing a heading insertion command when the point is at a heading. e.g.:
;;     * ‘C-c C-t 4’
;; will replace the current heading with a level-four heading.
;; 
;; - Section navigation via `outline-minor-mode', using the same keybindings as in org-mode:
;;     * ‘C-c C-f’
;;     * ‘C-c C-b’
;;     * ‘C-c C-u’
;;     * ‘C-c C-p’
;;     * ‘C-c C-n’
;; 
;; - Reorder list items with:
;;     * `M-<up>’
;;     * `M-<down>’
;; 

;; See:
;;     - https://jblevins.org/projects/markdown-mode/
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-markdown)