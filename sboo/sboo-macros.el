;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities that are Macros ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ~EMACS~ (FilePath)

  ;; Construct a filepath literal, relative to `user-emacs-directory'.
  ;; 
  ;; M-: (~EMACS ./lisp)
  ;; \"/home/sboo/.emacs.d/lisp"

  `(expand-file-name
    (concat (or user-emacs-directory "~/.emacs.d/")
            (symbol-name (quote ,FilePath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro load-path! (FilePath)

  ;; Register a filepath literal onto the `load-path'.
  ;; 
  ;; M-: (macroexpand (load-path! ./lisp))
  ;; (add-to-list 'load-path \"/home/sboo/.emacs.d/lisp\")

  `(add-to-list 'load-path
     (expand-file-name
       (concat (or user-emacs-directory "~/.emacs.d/")
               (symbol-name (quote ,FilePath))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro binding! (KeyString Command)
  `(global-set-key (kbd ,KeyString) (function ,Command)))

;; ^ e.g.
;;
;;   (binding! "M-r" query-replace)
;;    ==
;;   (global-set-key (kbd "M-r") #'query-replace)
;;

;TODO;(defmacro bind! (KeySequence Command &optional KeyMap) `(define-key ,KeyMap (kbd ,KeySequence) (function ,Command)))

;;          (global-set-key key binding)
;;          ==
;;          (define-key (current-global-map) key binding)
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html#Key-Binding-Commands
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; > server-running-p predicate will evaluate to t if the Emacs server is running, 
;; > irrespective of which Emacs session currently "owns" the server process.

;; See:
;;
;; - https://emacs.stackexchange.com/questions/31224/how-to-test-programmatically-whether-the-current-emacs-session-among-several

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-macros)