;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for InternalPackages only.
;;
;; That is:
;; - they're used by my configurations for InternalPackages;
;; - and they depend only on InternalPackages themselves.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-bind-key! (KEYMAP KEYBINDING)

  "Wraps `define-key' and `kbd'.

  (:: `KEYBINDING' (String, FunctionSymbol))
  
  i.e. « '(( \"...\" . #'... )) ...) ».
  "
  (interactive)

  (progn
    
    (let ((*keysequence* (car KEYBINDING))
          (*command*     (cdr KEYBINDING)))
      
    (define-key KEYMAP (kbd *keysequence*) *command*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-bind-key-globally! (KEYBINDING)

  "Wraps `global-set-key' and `kbd'.

  (:: `KEYBINDING' (String, FunctionSymbol))
  
  i.e. « ( \"...\" . #'... ) ».
  "
  (interactive)

  (progn
    
    (let ((*keysequence* (car KEYBINDING))
          (*command*     (cdr KEYBINDING)))
      
    (global-set-key (kbd *keysequence*) *command*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-bind-keys-for! (KEYMAP KEYBINDINGS)

  "Wraps `define-key' and `kbd'.

  (:: `KEYBINDINGS' [(String, FunctionSymbol)])
  
  i.e. « '(( \"...\" . #'... )) ...) ».
  "
  (interactive)

  (mapc (lambda (*keysequence*) (sboo-bind-key! KEYMAP *keysequence*))
        KEYBINDINGS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; `define-key'
;; 
;; a built-in function.
;;
;; signature:
;;   (define-key KEYMAP KEY DEF)
;;
;; In KEYMAP, define key sequence KEY as DEF.
;; KEYMAP is a keymap.
;;
;; KEY is a string or a vector of symbols and characters, representing a
;; sequence of keystrokes and events.  Non-ASCII characters with codes
;; above 127 (such as ISO Latin-1) can be represented by vectors.
;; Two types of vector have special meanings:
;;  [remap COMMAND] remaps any key binding for COMMAND.
;;  [t] creates a default definition, which applies to any event with no
;;     other definition in KEYMAP.
;;
;; DEF is anything that can be a key’s definition:
;;
;;  * `nil' (means key is undefined in this keymap),
;;  * a `command' (a Lisp function suitable for interactive calling),
;;  * a `string' (treated as a keyboard macro),
;;  * a `keymap' (to define a prefix key),
;;  * a `symbol' (when the key is looked up, the symbol will stand for its
;;     function definition, which should at that time be one of the above,
;;     or another symbol whose function definition is used, etc.),
;;  * a `cons' (STRING . DEFN), meaning that DEFN is the definition
;;     (DEFN should be a valid definition in its own right),
;;  or a cons (MAP . CHAR), meaning use definition of CHAR in keymap MAP,
;;  or an extended menu item definition.
;;
;; If KEYMAP is a sparse keymap with a binding for KEY, the existing
;; 
;; 

;; See:
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-utilities-internal)