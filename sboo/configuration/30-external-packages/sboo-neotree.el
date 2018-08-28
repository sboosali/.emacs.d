;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neotree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package neotree

  :init
  (progn

    (setq
     neo-smart-open t)
    ;; ^ Every time when the neotree window is opened,
    ;; let it find current file and jump to node.
    
    (setq
     projectile-switch-project-action 'neotree-projectile)
    ;; ^ Projectile integration: when running ‘projectile-switch-project’ (C-c p p),
    ;; ‘neotree’ will change root automatically.

    )

  :config
  (neotree-dir "~/haskell")
  ;; ^
  ;;   neotree-dir (path)
  ;;   "Show the NeoTree window, and change root to PATH."

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; [Usage] Neotree Buffer:
;;
;; n
;; next line
;;
;; p
;; previous line
;;
;;
;; SPC or RET or TAB
;; Open current item if it is a file. Fold/Unfold current item if it is a directory.
;;
;; U
;; Go up a directory
;;
;; g
;; Refresh
;;
;; A
;; Maximize/Minimize the NeoTree Window
;;
;; H
;; Toggle display hidden files
;;
;; C-c C-n
;; Create a file or create a directory if filename ends with a ‘/’
;;
;; C-c C-d
;; Delete a file or a directory.
;;
;; C-c C-r
;; Rename a file or a directory.
;;
;; C-c C-c
;; Change the root directory.
;;
;; C-c C-p
;; Copy a file or a directory.
;;

;; [Usage] Commands:
;;
;; neotree-enter
;; Open File / Unfold Directory.
;;
;; neotree-refresh
;; Refresh.
;;
;; neotree-stretch-toggle 
;; Maximize / Minimize.
;;
;; neotree-change-root 
;; Switch Root Directory.
;;
;; neotree-hidden-file-toggle 
;; Toggle hidden files.
;;
;; neotree-rename-node 
;; Rename a Node.
;;
;; neotree-delete-node 
;; Delete a Node.
;;
;; neotree-create-node 
;; Create a file; or if it ends with ‘/’, a directory.

;; See:
;;     - https://www.emacswiki.org/emacs/NeoTree
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-neotree)