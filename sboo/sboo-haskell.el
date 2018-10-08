;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require ')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hacks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun haskell-doc-current-info ()
  (when (commandp #'dante-type-at)
    (call-interactively #'dante-type-at)))

;; ^ `dante'+`eldoc' integration.
;;
;; NOTE disable if too slow.
;;
;; Alternatively:
;;
;; * `dante-info': more informative, pops-up a `*Help' buffer.
;; * `dante-type-at': more concise, prints to Echo Area (like `eldoc' should),
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun haskell-mode-after-save-handler ()
  (progn))

 ;; ^ HACK fixes this pseudo-error:
 ;;
 ;;     Error running timer ‘real-auto-save-buffers’: (void-function haskell-mode-after-save-handler)
 ;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dante ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-dante-cabal-new-repl (root)

  "Locate a `cabal.project' file.
  "
  (interactive)

  (when (or (directory-files root nil ".+\\.project$") (file-exists-p "cabal.project"))

    '("cabal" "new-repl" dante-target "--builddir=dist-newdante")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-dante-repl-command-line-methods-alist

  `((new-build . sboo-dante-cabal-new-repl)
    (stack     . ,(lambda (root) (dante-repl-by-file root '("stack.yaml") '("stack" "repl" dante-target))))
    (bare      . ,(lambda (_) '("cabal" "repl" dante-target "--builddir=dist/dante"))))

  "Override `dante-repl-command-line-methods-alist'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-dante-mode ()
  
  "Start/restart `dante'.
  
  (i.e. `dante-mode' or `dante-restart').
  "
  (interactive)

  (if (bound-and-true-p 'dante-mode)

    (dante-restart)

   (dante-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-haskell)