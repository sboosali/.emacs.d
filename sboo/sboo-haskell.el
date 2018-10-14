;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ElDoc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom sboo-haskell-eldoc nil

  "Which type-provider `sboo-haskell-doc-current-info' will call.

Each symbol represents a particular type (/ info / docs / etc) provider."

  :type '(choice (const nil)
                 (const dante
                        :tag "`dante-type-at'"))

  :safe  t
  :group 'sboo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-haskell-doc-current-info ()
  "Custom `haskell-doc-current-info'."

  (pcase sboo-haskell-eldoc
    
    ('dante

     (when (commandp #'dante-type-at)
       (dante-type-at nil)))

    ;; ^ (`dante-type-at' `nil') only echoes (c.f. doesn't insert).

    (_

     nil)))

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
;; Hacks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

    '("cabal" "new-repl" dante-target "--builddir=dist-dante")))

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

(defvar sboo-haskell-prettify-symbols-alist
  
  '(("::" . ?∷)
    ("=>" . ?⇒)
    ("->" . ?→)
    ("<-" . ?←)
    ("forall" . ?∀))

  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-haskell-prettify-symbols ()

  (interactive)

  (if prettify-symbols-mode

      (prettify-symbols-mode 0)

    (progn
      (setq-local prettify-symbols-alist sboo-haskell-prettify-symbols-alist)
      (prettify-symbols-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-haskell)