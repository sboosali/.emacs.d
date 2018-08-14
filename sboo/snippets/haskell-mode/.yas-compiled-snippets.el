;;; Compiled snippets and support files for `haskell-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'haskell-mode
                     '(("ps2" "  KeyDescription\n\n    { number      = $1\n    , character   = $0\n    , twinKeys    = def\n    , isModifier  = def\n    , isNumpad    = def\n    , side        = def\n    , codes1      = Codes { pressCode   = \"_\"\n                          , releaseCode = \"_\"\n                          }\n    , codes2      = Codes { pressCode   =    \"$2\"\n                          , releaseCode = \"F0 $2\"\n                          }\n    , codes3      = Codes { pressCode   =    \"$2\"\n                          , releaseCode = \"F0 $2\"\n                          }\n    }\n" "ScanCodes.PS2.KeyDescription { ... }" nil nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/x-scancode.yasnippet" nil nil)
                       ("opt" "{-# OPTIONS_GHC `(when (boundp 'haskell-ghc-supported-options)\n                   (cl-some #'(lambda (fn) (funcall fn \"GHC Option: \" haskell-ghc-supported-options))\n                         yas-prompt-functions))` #-}" "GHC options pragma"
                        (=
                         (length "opt")
                         (current-column))
                        nil nil "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/pragma-option.yasnippet" nil nil)
                       ("lang" "{-# LANGUAGE `(when (boundp 'haskell-ghc-supported-extensions)\n                (cl-some #'(lambda (fn) (funcall fn \"Extension: \" haskell-ghc-supported-extensions))\n                      yas-prompt-functions))` #-}" "language extension pragma"
                        (=
                         (length "lang")
                         (current-column))
                        nil nil "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/pragma-language.yasnippet" nil nil)
                       ("new" "newtype ${1:Type} ${2:a} = $1 { un$1 :: ${3:a} } ${4:deriving (${5:Show, Eq})}" "newtype"
                        (=
                         (length "new")
                         (current-column))
                        nil nil "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/newtype.yasnippet" nil nil)
                       ("new-text" "--------------------------------------------------\n\n{-|\n\n-}\n\nnewtype $1 = $1\n\n  Text$0\n\n  deriving stock    (Show,Read,Generic)\n  deriving newtype  (Eq,Ord,Semigroup,Monoid)\n  deriving newtype  (NFData,Hashable)\n\ninstance IsString $1 where\n  fromString = (coerce . fromString)\n\n" "[sboo] a Text-`newtype`."
                        (=
                         (length "new-text")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/newtype-text.yasnippet" nil nil)
                       ("new-string" "--------------------------------------------------\n\n{-|\n\n-}\n\nnewtype $1 = $1\n\n  String$0\n\n  deriving stock    (Show,Read,Lift,Generic)\n  deriving newtype  (Eq,Ord,Semigroup,Monoid)\n  deriving newtype  (NFData,Hashable)\n\ninstance IsString $1 where\n  fromString = coerce\n\n" "[sboo] a String-`newtype`."
                        (=
                         (length "new-string")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/newtype-string.yasnippet" nil nil)
                       ("new-number" "--------------------------------------------------\n\n{-|\n\n-}\n\nnewtype $1 = $1\n\n  Int$0\n\n  deriving stock    (Show,Read,Lift,Generic)\n  deriving newtype  (Eq,Ord,Num)\n  deriving newtype  (NFData,Hashable)\n\n-- instance Semigroup $1 where (<>)   = _\n-- instance Monoid    $1 where mempty = _\n\n" "[sboo] a `Num`-newtype."
                        (=
                         (length "new-number")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/newtype-number.yasnippet" nil nil)
                       ("new-list" "--------------------------------------------------\n\n{-|\n\n-}\n\nnewtype $1 = $1\n\n  [$2]\n\n  deriving stock    (Show,Read,Lift,Generic)\n  deriving newtype  (Eq,Ord,Semigroup,Monoid)\n  deriving newtype  (NFData,Hashable)\n\ninstance IsList $1 where\n  type Item $1 = $2\n  fromList = coerce\n  toList   = coerce\n\n" "[sboo] a List-`newtype` of some Item."
                        (=
                         (length "new-list")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/newtype-list.yasnippet" nil nil)
                       ("mod-standard" "--------------------------------------------------\n--------------------------------------------------\n\n{-|\n\n\n\n-}\n\nmodule $1.Types where\n\n--------------------------------------------------\n\nimport Prelude_natlink\n\n--------------------------------------------------\n\n\n\n--------------------------------------------------\n--------------------------------------------------\n\n{-| \n\n-}\n\n$0\n\n--------------------------------------------------\n\n{-| \n\n-}\n\n--------------------------------------------------\n\n{-| \n\n-}\n\n--------------------------------------------------\n\n{-| \n\n-}\n\n--------------------------------------------------\n\n{-| \n\n-}\n\n--------------------------------------------------\n--------------------------------------------------" "[sboo] a `Module` declaration, with textual sections."
                        (=
                         (length "mod-standard")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/module-header.yasnippet" nil nil)
                       ("opt" "{-# OPTIONS_GHC `(when (boundp 'haskell-ghc-supported-options)\n                   (cl-some #'(lambda (fn) (funcall fn \"GHC Option: \" haskell-ghc-supported-options))\n                         yas-prompt-functions))` #-}" "GHC options pragma"
                        (=
                         (length "opt")
                         (current-column))
                        nil nil "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/hs-option-pragma.yasnippet" nil nil)
                       ("mod" "module ${1:`(if (not buffer-file-name) \"Module\"\n                (let ((name (file-name-sans-extension (buffer-file-name)))\n                      (case-fold-search nil))\n                     (if (cl-search \"src/\" name)\n                         (replace-regexp-in-string \"/\" \".\"\n                           (replace-regexp-in-string \"^\\/[^A-Z]*\" \"\"\n                             (car (last (split-string name \"src\")))))\n                         (file-name-nondirectory name))))`}\n    ( ${3:export}\n    ${4:, ${5:export}}\n    ) where\n\n$0" "exports module"
                        (=
                         (length "mod")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/hs-module-exports.yasnippet" nil nil)
                       ("main" "module Main where\n\nmain :: IO ()\nmain = do\n  ${1:undefined}$0\n  return ()" "main module"
                        (=
                         (length "main")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/hs-main.yasnippet" nil nil)
                       ("lang" "{-# LANGUAGE `(when (boundp 'haskell-ghc-supported-extensions)\n                (cl-some #'(lambda (fn) (funcall fn \"Extension: \" haskell-ghc-supported-extensions))\n                      yas-prompt-functions))` #-}" "language extension pragma"
                        (=
                         (length "lang")
                         (current-column))
                        nil nil "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/hs-lang-pragma.yasnippet" nil nil)
                       ("{-" "{- $0 -}" "block comment" nil nil nil "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/hs-comment-block.yasnippet" nil nil)
                       ("der-standard" "  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)\n  deriving anyclass ($0NFData,Hashable)\n" "[sboo] `deriving` \"standard\" typeclasses (for most types)."
                        (=
                         (length "der-standard")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/deriving-standard.yasnippet" nil nil)
                       ("der-newtype" "  deriving stock    (Show,Read,Lift,Generic)\n  deriving newtype  (Eq,Ord)\n  deriving newtype  (NFData,Hashable)\n$0" "[sboo] `deriving` \"standard\" typeclasses (for a `newtype`)."
                        (=
                         (length "der-newtype")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/deriving-newtype.yasnippet" nil nil)
                       ("der-functor" "  deriving stock    (Functor,Foldable,Traversable)\n  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)\n  deriving anyclass (NFData,Hashable)\n" "[sboo] `deriving` instances for a unary `Functor`."
                        (=
                         (length "der-functor")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/deriving-functor.yasnippet" nil nil)
                       ("der-enum" "  deriving stock    (Enum,Bounded,Ix)\n  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)\n  deriving anyclass (Enumerable,$0NFData,Hashable)\n" "[sboo] `deriving` an `Enum` type."
                        (=
                         (length "der-enum")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/deriving-enum.yasnippet" nil nil)
                       ("data-record" "--------------------------------------------------\n\n{-|  $0\n\n-}\n\ndata $1 = $1\n  { $2 :: $3\n  , $4 :: $5\n  }\n\n  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)\n  deriving anyclass (NFData,Hashable)\n\n-- | @= 'sappendGeneric'@ (i.e. pair-wise).\ninstance Semigroup $1 where\n  (<>) = sappendGeneric\n\n-- -- | @= 'memptyGeneric'@\n-- instance Monoid $1 where\n--   mempty = memptyGeneric\n-- \n-- -- | @= 'default$1'@\n-- instance Default $1 where\n--   def = default$1\n-- \n-- {-|\n-- \n-- -}\n-- \n-- default$1 :: $1\n-- default$1 = $1{..}\n--   where\n--   $2 = def\n--   $4 = def\n\n" "[sboo] define a record (i.e. product) `data`type."
                        (=
                         (length "data-record")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/data-record.yasnippet" nil nil)
                       ("data-enum" "--------------------------------------------------\n\n{-|\n\n-}\n\ndata $1\n\n  = $2\n  | $3\n\n  deriving stock    (Enum,Bounded,Ix)\n  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)\n  deriving anyclass (Enumerable)\n  deriving anyclass (NFData,Hashable)\n\n" "[sboo] define an enumeration (i.e. sum) `data`type."
                        (=
                         (length "data-enum")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/data-enum.yasnippet" nil nil)
                       ("def" "-- | @= 'default$1'@\ninstance Default $1 where\n  def = default$1\n\n-- | @= '$2'@\ndefault$1 :: $1\ndefault$1 = $2\n\n$0" "[sboo] `Default` instance and a constant."
                        (=
                         (length "def")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/haskell-mode/binding-default.yasnippet" nil nil)))


;;; Do not edit! File generated at Tue Aug 14 02:36:53 2018
