;;; Compiled snippets and support files for `haskell-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'haskell-mode
                     '(("opt" "{-# OPTIONS_GHC `(when (boundp 'haskell-ghc-supported-options)\n                   (cl-some #'(lambda (fn) (funcall fn \"GHC Option: \" haskell-ghc-supported-options))\n                         yas-prompt-functions))` #-}" "GHC options pragma"
                        (=
                         (length "opt")
                         (current-column))
                        nil nil "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/option-pragma.yasnippet" nil nil)
                       ("new" "newtype ${1:Type} ${2:a} = $1 { un$1 :: ${3:a} } ${4:deriving (${5:Show, Eq})}" "newtype"
                        (=
                         (length "new")
                         (current-column))
                        nil nil "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/newtype.yasnippet" nil nil)
                       ("newstring" "--------------------------------------------------\n\n{-|\n\n-}\n\nnewtype $1 = $1\n\n  String$0\n\n  deriving stock    (Show,Read,Lift,Generic)\n  deriving newtype  (Eq,Ord,Semigroup,Monoid)\n  deriving newtype  (NFData,Hashable)\n\ninstance IsString $1 where fromString = coerce\n\n" "a String-`newtype`."
                        (=
                         (length "newstring")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/newtype-string.yasnippet" nil nil)
                       ("newnum" "--------------------------------------------------\n\n{-|\n\n-}\n\nnewtype $1 = $1\n\n  Int$0\n\n  deriving stock    (Show,Read,Lift,Generic)\n  deriving newtype  (Eq,Ord,Num)\n  deriving newtype  (NFData,Hashable)\n\n-- instance Semigroup $1 where (<>)   = _\n-- instance Monoid    $1 where mempty = _\n\n" "a `Num`-newtype."
                        (=
                         (length "newnum")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/newtype-number.yasnippet" nil nil)
                       ("newlist" "--------------------------------------------------\n\n{-|\n\n-}\n\nnewtype $1 = $1\n\n  [$2]\n\n  deriving stock    (Show,Read,Lift,Generic)\n  deriving newtype  (Eq,Ord,Semigroup,Monoid)\n  deriving newtype  (NFData,Hashable)\n\ninstance IsList $1 where\n  type Item $1 = $2\n  fromList = coerce\n  toList   = coerce\n\n" "a List-`newtype` of some Item."
                        (=
                         (length "newlist")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/newtype-list.yasnippet" nil nil)
                       ("mod" "module ${1:`(if (not buffer-file-name) \"Module\"\n                (let ((name (file-name-sans-extension (buffer-file-name)))\n                      (case-fold-search nil))\n                     (if (cl-search \"src/\" name)\n                         (replace-regexp-in-string \"/\" \".\"\n                           (replace-regexp-in-string \"^\\/[^A-Z]*\" \"\"\n                             (car (last (split-string name \"src\")))))\n                         (file-name-nondirectory name))))`}\n    ( ${3:export}\n    ${4:, ${5:export}}\n    ) where\n\n$0" "exports module"
                        (=
                         (length "mod")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/module-exports.yasnippet" nil nil)
                       ("main" "module Main where\n\nmain :: IO ()\nmain = do\n  ${1:undefined}$0\n  return ()" "main module"
                        (=
                         (length "main")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/main.yasnippet" nil nil)
                       ("lang" "{-# LANGUAGE `(when (boundp 'haskell-ghc-supported-extensions)\n                (cl-some #'(lambda (fn) (funcall fn \"Extension: \" haskell-ghc-supported-extensions))\n                      yas-prompt-functions))` #-}" "language extension pragma"
                        (=
                         (length "lang")
                         (current-column))
                        nil nil "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/lang-pragma.yasnippet" nil nil)
                       ("functor" "  deriving stock    (Functor,Foldable,Traversable)\n  deriving stock    (Show,Read,Eq,Ord,Enum,Bounded,Ix,Lift,Generic)\n  deriving anyclass (NFData,Hashable)\n" "`deriving` a unary `Functor` type."
                        (=
                         (length "functor")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/deriving-functor.yasnippet" nil nil)
                       ("enum" "  deriving stock    (Enum,Bounded,Ix)\n  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)\n  deriving anyclass (Enumerable,$0NFData,Hashable)\n" "`deriving` an `Enum` type."
                        (=
                         (length "enum")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/deriving-enum.yasnippet" nil nil)
                       ("def" "-- | @= 'default$1'@\ninstance Default $1 where\n  def = default$1\n\n-- | @= '$2'@\ndefault$1 :: $1\ndefault$1 = $2\n\n$0" "`Default` instance and a constant."
                        (=
                         (length "def")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/default.yasnippet" nil nil)
                       ("{-" "{- $0 -}" "block comment" nil nil nil "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/comment-block.yasnippet" nil nil)))


;;; Do not edit! File generated at Thu Aug  9 16:47:30 2018
