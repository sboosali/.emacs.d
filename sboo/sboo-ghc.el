;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for the `ghc' package.
;;
;; See:
;;
;; * `sboo-excluded-directories'.
;; * `sboo-excluded-file-names'
;; * `sboo-excluded-file-extensions'.
;;
;; 
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(require 'cl)
(require 'pcase)

;;

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;

(defun sboo-ghc-get-name-of-prior-definition ()
 
  "Return the name of the previous haskell definition (from `point').

Output:

• a string.

Calls `haskell-ds-move-to-decl'."

  (save-excursion

    (let ((is-direction-forwards?  nil)
          (is-haskell-literate?    nil)
          (is-movement-idempotent? t)
          )
      (haskell-ds-move-to-decl is-direction-forwards? is-haskell-literate? is-movement-idempotent?))

    (thing-at-point 'symbol :no-properties)))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-ghc-pragma-read ()

  "Read a GHC pragma, plus any of that pragma's required inputs.

Output:

• a list of strings.

Related:

• `sboo-ghc-pragma-read-pragma'.
• `sboo-ghc-pragmas-alist'."

  (interactive)

  (completing-read (format "%s: " prompt)
                   candidates))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-pragma ()

  "Read a GHC pragma.

Related:

• `sboo-ghc-pragmas-alist'."

  (interactive)

  (let ((prompt     "Pragma")
        (candidates sboo-ghc-pragmas)
        )

    (completing-read (format "%s: " prompt)
                     candidates)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read- ()

  "Read a .

Inputs:

• 

Output:

• 

Related:

• `sboo-ghc-pragmas-alist'."

  (interactive)

  (let ((prompt     "")
        (candidates )
        )

  (sboo-ghc-pragma-read prompt candidates)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-string ()

  "Read a haskell string.

Escapes:

• double-quotes (i.e. « \" »).

Related:

• `sboo-ghc-pragmas-alist'."

  (interactive)

  (let ((prompt     "String")
        (candidates nil)
        )

  (sboo-ghc-pragma-read prompt candidates)))

;;----------------------------------------------;;

(defun sboo-ghc-read-haskell-variable ()

  "Read a haskell variable.

Inputs:

• 

Output:

• 

Related:

• `sboo-ghc-pragmas-alist'."

  (interactive)

  (let ((prompt     "")
        (candidates )
        )

  (sboo-ghc-pragma-read prompt candidates)))

;;----------------------------------------------;;

;;----------------------------------------------;;

(defun sboo-ghc-pragma-insert-pragma (&optional pragma)

  "Insert a GHC pragma, reading it if PRAGMA is nil.

Related:

• `sboo-ghc-pragma-read-pragma'."

  (interactive (list
                (sboo-ghc-pragma-read-pragma)))

  (let* ((STRING (format "{-# %s #-}" pragma))
         )

    (insert STRING)))

;;----------------------------------------------;;

;;----------------------------------------------;;
;; Aliases -------------------------------------;;
;;----------------------------------------------;;

(defalias 'sboo-ghc-read-WARNING    #'sboo-ghc-read-string)
(defalias 'sboo-ghc-read-DEPRECATED #'sboo-ghc-read-string)

     ;; ("LANGUAGE"     . sboo-ghc-read-LANGUAGE)

     ;; ("OPTIONS_GHC"  . sboo-ghc-read-OPTIONS_GHC)

     ;; ("MINIMAL"      . sboo-ghc-read-MINIMAL)
     ;; ("COMPLETE"     . sboo-ghc-read-COMPLETE)

     ;; ("INLINE"       . sboo-ghc-read-INLINE)
     ;; ("NOINLINE"     . sboo-ghc-read-INLINE)
     ;; ("INLINABLE"    . sboo-ghc-read-INLINABLE)
     ;; ("CONLIKE"      . sboo-ghc-read-CONLIKE)

     ;; ("RULES"        . sboo-ghc-read-RULES)

     ;; ("SPECIALIZE"   . sboo-ghc-read-SPECIALIZE)

     ;; ("OVERLAPPING"  . sboo-ghc-read-OVERLAPPING)
     ;; ("OVERLAPPABLE" . sboo-ghc-read-OVERLAPPABLE)
     ;; ("OVERLAPS"     . sboo-ghc-read-OVERLAPS)
     ;; ("INCOHERENT"   . sboo-ghc-read-INCOHERENT)

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;

(defcustom sboo-ghc-pragmas-alist ;TODO; make internal variable « hashtable ».

  '( ("LANGUAGE"     . sboo-ghc-read-LANGUAGE)

     ("OPTIONS_GHC"  . sboo-ghc-read-OPTIONS_GHC)

  ;; ("INCLUDE"      . sboo-ghc-read-INCLUDE)

     ("WARNING"      . sboo-ghc-read-WARNING)
     ("DEPRECATED"   . sboo-ghc-read-DEPRECATED)

     ("MINIMAL"      . sboo-ghc-read-MINIMAL)
     ("COMPLETE"     . sboo-ghc-read-COMPLETE)

     ("INLINE"       . sboo-ghc-read-INLINE)
     ("NOINLINE"     . sboo-ghc-read-INLINE)
     ("INLINABLE"    . sboo-ghc-read-INLINABLE)
     ("CONLIKE"      . sboo-ghc-read-CONLIKE)

     ("RULES"        . sboo-ghc-read-RULES)

     ("SPECIALIZE"   . sboo-ghc-read-SPECIALIZE)

     ("UNPACK"       . nil)
     ("NOUNPACK"     . nil)

     ("OVERLAPPING"  . sboo-ghc-read-OVERLAPPING)
     ("OVERLAPPABLE" . sboo-ghc-read-OVERLAPPABLE)
     ("OVERLAPS"     . sboo-ghc-read-OVERLAPS)
     ("INCOHERENT"   . sboo-ghc-read-INCOHERENT)

     ;; ("LINE"         . sboo-ghc-read-LINE)
     ;; ("COLUMN"       . sboo-ghc-read-COLUMN)
     ;; ("SOURCE"       . sboo-ghc-read-SOURCE)
     )

  "GHC Pragmas and elisp commands (to read them from the user)."

  :type '(alist :key-type   (string   :tag "Pragma")
                :value-type '(choice (const nil)
                                     (function :tag "Completer/Reader"))
                )

  :safe t

  :group 'sboo)

;;----------------------------------------------;;

(defcustom sboo-ghc-pragmas

  '( "LANGUAGE"
     "OPTIONS_GHC"
     "INCLUDE"
     "WARNING"
     "DEPRECATED"
     "MINIMAL"
     "INLINE"
     "NOINLINE"
     "INLINABLE"
     "CONLIKE"
     "LINE"
     "COLUMN"
     "RULES"
     "SPECIALIZE"
     "UNPACK"
     "NOUNPACK"
     "SOURCE"
     "COMPLETE"
     "OVERLAPPING"
     "OVERLAPPABLE"
     "OVERLAPS"
     "INCOHERENT"
     )

  "GHC Pragmas

i.e.  « {-# ... #-} »."

  :type '(list (string))

  :safe t

  :group 'sboo)

;;----------------------------------------------;;

(defcustom sboo-ghc-language-extensions

  '( "AllowAmbiguousTypes"
     "AlternativeLayoutRule"
     "AlternativeLayoutRuleTransitional"
     "ApplicativeDo"
     "Arrows"
     "AutoDeriveTypeable"
     "BangPatterns"
     "BinaryLiterals"
     "CApiFFI"
     "CPP"
     "ConstrainedClassMethods"
     "ConstraintKinds"
     "DataKinds"
     "DatatypeContexts"
     "DefaultSignatures"
     "DeriveAnyClass"
     "DeriveDataTypeable"
     "DeriveFoldable"
     "DeriveFunctor"
     "DeriveGeneric"
     "DeriveLift"
     "DeriveTraversable"
     "DisambiguateRecordFields"
     "DoAndIfThenElse"
     "DoRec"
     "DuplicateRecordFields"
     "EmptyCase"
     "EmptyDataDecls"
     "ExistentialQuantification"
     "ExplicitForAll"
     "ExplicitNamespaces"
     "ExtendedDefaultRules"
     "FlexibleContexts"
     "FlexibleInstances"
     "ForeignFunctionInterface"
     "FunctionalDependencies"
     "GADTSyntax"
     "GADTs"
     "GHCForeignImportPrim"
     "GeneralizedNewtypeDeriving"
     "Haskell2010"
     "Haskell98"
     "ImplicitParams"
     "ImplicitPrelude"
     "ImpredicativeTypes"
     "IncoherentInstances"
     "InstanceSigs"
     "InterruptibleFFI"
     "JavaScriptFFI"
     "KindSignatures"
     "LambdaCase"
     "LiberalTypeSynonyms"
     "MagicHash"
     "MonadComprehensions"
     "MonadFailDesugaring"
     "MonoLocalBinds"
     "MonoPatBinds"
     "MonomorphismRestriction"
     "MultiParamTypeClasses"
     "MultiWayIf"
     "NPlusKPatterns"
     "NamedFieldPuns"
     "NamedWildCards"
     "NegativeLiterals"
     "NoAllowAmbiguousTypes"
     "NoAlternativeLayoutRule"
     "NoAlternativeLayoutRuleTransitional"
     "NoApplicativeDo"
     "NoArrows"
     "NoAutoDeriveTypeable"
     "NoBangPatterns"
     "NoBinaryLiterals"
     "NoCApiFFI"
     "NoCPP"
     "NoConstrainedClassMethods"
     "NoConstraintKinds"
     "NoDataKinds"
     "NoDatatypeContexts"
     "NoDefaultSignatures"
     "NoDeriveAnyClass"
     "NoDeriveDataTypeable"
     "NoDeriveFoldable"
     "NoDeriveFunctor"
     "NoDeriveGeneric"
     "NoDeriveLift"
     "NoDeriveTraversable"
     "NoDisambiguateRecordFields"
     "NoDoAndIfThenElse"
     "NoDoRec"
     "NoDuplicateRecordFields"
     "NoEmptyCase"
     "NoEmptyDataDecls"
     "NoExistentialQuantification"
     "NoExplicitForAll"
     "NoExplicitNamespaces"
     "NoExtendedDefaultRules"
     "NoFlexibleContexts"
     "NoFlexibleInstances"
     "NoForeignFunctionInterface"
     "NoFunctionalDependencies"
     "NoGADTSyntax"
     "NoGADTs"
     "NoGHCForeignImportPrim"
     "NoGeneralizedNewtypeDeriving"
     "NoImplicitParams"
     "NoImplicitPrelude"
     "NoImpredicativeTypes"
     "NoIncoherentInstances"
     "NoInstanceSigs"
     "NoInterruptibleFFI"
     "NoJavaScriptFFI"
     "NoKindSignatures"
     "NoLambdaCase"
     "NoLiberalTypeSynonyms"
     "NoMagicHash"
     "NoMonadComprehensions"
     "NoMonadFailDesugaring"
     "NoMonoLocalBinds"
     "NoMonoPatBinds"
     "NoMonomorphismRestriction"
     "NoMultiParamTypeClasses"
     "NoMultiWayIf"
     "NoNPlusKPatterns"
     "NoNamedFieldPuns"
     "NoNamedWildCards"
     "NoNegativeLiterals"
     "NoNondecreasingIndentation"
     "NoNullaryTypeClasses"
     "NoNumDecimals"
     "NoOverlappingInstances"
     "NoOverloadedLabels"
     "NoOverloadedLists"
     "NoOverloadedStrings"
     "NoPackageImports"
     "NoParallelArrays"
     "NoParallelListComp"
     "NoPartialTypeSignatures"
     "NoPatternGuards"
     "NoPatternSignatures"
     "NoPatternSynonyms"
     "NoPolyKinds"
     "NoPolymorphicComponents"
     "NoPostfixOperators"
     "NoQuasiQuotes"
     "NoRank2Types"
     "NoRankNTypes"
     "NoRebindableSyntax"
     "NoRecordPuns"
     "NoRecordWildCards"
     "NoRecursiveDo"
     "NoRelaxedLayout"
     "NoRelaxedPolyRec"
     "NoRoleAnnotations"
     "NoScopedTypeVariables"
     "NoStandaloneDeriving"
     "NoStaticPointers"
     "NoStrict"
     "NoStrictData"
     "NoTemplateHaskell"
     "NoTemplateHaskellQuotes"
     "NoTraditionalRecordSyntax"
     "NoTransformListComp"
     "NoTupleSections"
     "NoTypeApplications"
     "NoTypeFamilies"
     "NoTypeFamilyDependencies"
     "NoTypeInType"
     "NoTypeOperators"
     "NoTypeSynonymInstances"
     "NoUnboxedTuples"
     "NoUndecidableInstances"
     "NoUndecidableSuperClasses"
     "NoUnicodeSyntax"
     "NoUnliftedFFITypes"
     "NoViewPatterns"
     "NondecreasingIndentation"
     "NullaryTypeClasses"
     "NumDecimals"
     "OverlappingInstances"
     "OverloadedLabels"
     "OverloadedLists"
     "OverloadedStrings"
     "PackageImports"
     "ParallelArrays"
     "ParallelListComp"
     "PartialTypeSignatures"
     "PatternGuards"
     "PatternSignatures"
     "PatternSynonyms"
     "PolyKinds"
     "PolymorphicComponents"
     "PostfixOperators"
     "QuasiQuotes"
     "Rank2Types"
     "RankNTypes"
     "RebindableSyntax"
     "RecordPuns"
     "RecordWildCards"
     "RecursiveDo"
     "RelaxedLayout"
     "RelaxedPolyRec"
     "RoleAnnotations"
     "Safe"
     "ScopedTypeVariables"
     "StandaloneDeriving"
     "StaticPointers"
     "Strict"
     "StrictData"
     "TemplateHaskell"
     "TemplateHaskellQuotes"
     "TraditionalRecordSyntax"
     "TransformListComp"
     "Trustworthy"
     "TupleSections"
     "TypeApplications"
     "TypeFamilies"
     "TypeFamilyDependencies"
     "TypeInType"
     "TypeOperators"
     "TypeSynonymInstances"
     "UnboxedTuples"
     "UndecidableInstances"
     "UndecidableSuperClasses"
     "UnicodeSyntax"
     "UnliftedFFITypes"
     "Unsafe"
     "ViewPatterns"
     )

  "GHC language extensions.

i.e. « -X... »."

  :type '(list (string))

  :safe t

  :group 'sboo)

;;----------------------------------------------;;

(defcustom sboo-ghc-compiler-options

  '( "-#include"
     "--abi-hash"
     "--frontend"
     "--help"
     "--info"
     "--interactive"
     "--make"
     "--numeric-version"
     "--print-booter-version"
     "--print-build-platform"
     "--print-c-compiler-flags"
     "--print-c-compiler-link-flags"
     "--print-debug-on"
     "--print-gcc-linker-flags"
     "--print-global-package-db"
     "--print-have-interpreter"
     "--print-have-native-code-generator"
     "--print-host-platform"
     "--print-ld-flags"
     "--print-ld-linker-flags"
     "--print-leading-underscore"
     "--print-libdir"
     "--print-object-splitting-supported"
     "--print-project-git-commit-id"
     "--print-project-version"
     "--print-rts-ways"
     "--print-stage"
     "--print-support-smp"
     "--print-tables-next-to-code"
     "--print-target-platform"
     "--print-unregisterised"
     "--show-iface"
     "--show-options"
     "--show-packages"
     "--supported-extensions"
     "--supported-languages"
     "--version"
     "-?"
     "-C"
     "-D"
     "-E"
     "-F"
     "-H"
     "-I"
     "-L"
     "-M"
     "-O"
     "-Odph"
     "-Onot"
     "-Rghc-timing"
     "-S"
     "-U"
     "-V"
     "-W"
     "-Wall"
     "-Wall-missed-specialisations"
     "-Walternative-layout-rule-transitional"
     "-Wamp"
     "-Wauto-orphans"
     "-Wcompat"
     "-Wcontext-quantification"
     "-Wdefault"
     "-Wdeferred-type-errors"
     "-Wdeprecated-flags"
     "-Wdeprecations"
     "-Wderiving-typeable"
     "-Wdodgy-exports"
     "-Wdodgy-foreign-imports"
     "-Wdodgy-imports"
     "-Wduplicate-constraints"
     "-Wduplicate-exports"
     "-Wempty-enumerations"
     "-Werror"
     "-Weverything"
     "-Wextra"
     "-Whi-shadowing"
     "-Widentities"
     "-Wimplicit-prelude"
     "-Wincomplete-patterns"
     "-Wincomplete-record-updates"
     "-Wincomplete-uni-patterns"
     "-Winline-rule-shadowing"
     "-Wmissed-specialisations"
     "-Wmissing-exported-signatures"
     "-Wmissing-exported-sigs"
     "-Wmissing-fields"
     "-Wmissing-import-lists"
     "-Wmissing-local-signatures"
     "-Wmissing-local-sigs"
     "-Wmissing-methods"
     "-Wmissing-monadfail-instances"
     "-Wmissing-pattern-synonym-signatures"
     "-Wmissing-signatures"
     "-Wmonomorphism-restriction"
     "-Wname-shadowing"
     "-Wno-all"
     "-Wno-all-missed-specialisations"
     "-Wno-alternative-layout-rule-transitional"
     "-Wno-amp"
     "-Wno-auto-orphans"
     "-Wno-compat"
     "-Wno-context-quantification"
     "-Wno-default"
     "-Wno-deferred-type-errors"
     "-Wno-deprecated-flags"
     "-Wno-deprecations"
     "-Wno-deriving-typeable"
     "-Wno-dodgy-exports"
     "-Wno-dodgy-foreign-imports"
     "-Wno-dodgy-imports"
     "-Wno-duplicate-constraints"
     "-Wno-duplicate-exports"
     "-Wno-empty-enumerations"
     "-Wno-everything"
     "-Wno-extra"
     "-Wno-hi-shadowing"
     "-Wno-identities"
     "-Wno-implicit-prelude"
     "-Wno-incomplete-patterns"
     "-Wno-incomplete-record-updates"
     "-Wno-incomplete-uni-patterns"
     "-Wno-inline-rule-shadowing"
     "-Wno-missed-specialisations"
     "-Wno-missing-exported-signatures"
     "-Wno-missing-exported-sigs"
     "-Wno-missing-fields"
     "-Wno-missing-import-lists"
     "-Wno-missing-local-signatures"
     "-Wno-missing-local-sigs"
     "-Wno-missing-methods"
     "-Wno-missing-monadfail-instances"
     "-Wno-missing-pattern-synonym-signatures"
     "-Wno-missing-signatures"
     "-Wno-monomorphism-restriction"
     "-Wno-name-shadowing"
     "-Wno-noncanonical-monad-instances"
     "-Wno-noncanonical-monadfail-instances"
     "-Wno-noncanonical-monoid-instances"
     "-Wno-orphans"
     "-Wno-overflowed-literals"
     "-Wno-overlapping-patterns"
     "-Wno-partial-type-signatures"
     "-Wno-redundant-constraints"
     "-Wno-safe"
     "-Wno-semigroup"
     "-Wno-tabs"
     "-Wno-trustworthy-safe"
     "-Wno-type-defaults"
     "-Wno-typed-holes"
     "-Wno-unrecognised-pragmas"
     "-Wno-unrecognised-warning-flags"
     "-Wno-unsafe"
     "-Wno-unsupported-calling-conventions"
     "-Wno-unsupported-llvm-version"
     "-Wno-unticked-promoted-constructors"
     "-Wno-unused-binds"
     "-Wno-unused-do-bind"
     "-Wno-unused-foralls"
     "-Wno-unused-imports"
     "-Wno-unused-local-binds"
     "-Wno-unused-matches"
     "-Wno-unused-pattern-binds"
     "-Wno-unused-top-binds"
     "-Wno-unused-type-patterns"
     "-Wno-warnings-deprecations"
     "-Wno-wrong-do-bind"
     "-Wnoncanonical-monad-instances"
     "-Wnoncanonical-monadfail-instances"
     "-Wnoncanonical-monoid-instances"
     "-Wnot"
     "-Worphans"
     "-Woverflowed-literals"
     "-Woverlapping-patterns"
     "-Wpartial-type-signatures"
     "-Wredundant-constraints"
     "-Wsafe"
     "-Wsemigroup"
     "-Wtabs"
     "-Wtrustworthy-safe"
     "-Wtype-defaults"
     "-Wtyped-holes"
     "-Wunrecognised-pragmas"
     "-Wunrecognised-warning-flags"
     "-Wunsafe"
     "-Wunsupported-calling-conventions"
     "-Wunsupported-llvm-version"
     "-Wunticked-promoted-constructors"
     "-Wunused-binds"
     "-Wunused-do-bind"
     "-Wunused-foralls"
     "-Wunused-imports"
     "-Wunused-local-binds"
     "-Wunused-matches"
     "-Wunused-pattern-binds"
     "-Wunused-top-binds"
     "-Wunused-type-patterns"
     "-Wwarn"
     "-Wwarnings-deprecations"
     "-Wwrong-do-bind"
     "-XAllowAmbiguousTypes"
     "-XAlternativeLayoutRule"
     "-XAlternativeLayoutRuleTransitional"
     "-XApplicativeDo"
     "-XArrows"
     "-XAutoDeriveTypeable"
     "-XBangPatterns"
     "-XBinaryLiterals"
     "-XCApiFFI"
     "-XCPP"
     "-XConstrainedClassMethods"
     "-XConstraintKinds"
     "-XDataKinds"
     "-XDatatypeContexts"
     "-XDefaultSignatures"
     "-XDeriveAnyClass"
     "-XDeriveDataTypeable"
     "-XDeriveFoldable"
     "-XDeriveFunctor"
     "-XDeriveGeneric"
     "-XDeriveLift"
     "-XDeriveTraversable"
     "-XDisambiguateRecordFields"
     "-XDoAndIfThenElse"
     "-XDoRec"
     "-XDuplicateRecordFields"
     "-XEmptyCase"
     "-XEmptyDataDecls"
     "-XExistentialQuantification"
     "-XExplicitForAll"
     "-XExplicitNamespaces"
     "-XExtendedDefaultRules"
     "-XFlexibleContexts"
     "-XFlexibleInstances"
     "-XForeignFunctionInterface"
     "-XFunctionalDependencies"
     "-XGADTSyntax"
     "-XGADTs"
     "-XGHCForeignImportPrim"
     "-XGeneralizedNewtypeDeriving"
     "-XGenerics"
     "-XHaskell2010"
     "-XHaskell98"
     "-XImplicitParams"
     "-XImplicitPrelude"
     "-XImpredicativeTypes"
     "-XIncoherentInstances"
     "-XInstanceSigs"
     "-XInterruptibleFFI"
     "-XJavaScriptFFI"
     "-XKindSignatures"
     "-XLambdaCase"
     "-XLiberalTypeSynonyms"
     "-XMagicHash"
     "-XMonadComprehensions"
     "-XMonadFailDesugaring"
     "-XMonoLocalBinds"
     "-XMonoPatBinds"
     "-XMonomorphismRestriction"
     "-XMultiParamTypeClasses"
     "-XMultiWayIf"
     "-XNPlusKPatterns"
     "-XNamedFieldPuns"
     "-XNamedWildCards"
     "-XNegativeLiterals"
     "-XNoAllowAmbiguousTypes"
     "-XNoAlternativeLayoutRule"
     "-XNoAlternativeLayoutRuleTransitional"
     "-XNoApplicativeDo"
     "-XNoArrows"
     "-XNoAutoDeriveTypeable"
     "-XNoBangPatterns"
     "-XNoBinaryLiterals"
     "-XNoCApiFFI"
     "-XNoCPP"
     "-XNoConstrainedClassMethods"
     "-XNoConstraintKinds"
     "-XNoDataKinds"
     "-XNoDatatypeContexts"
     "-XNoDefaultSignatures"
     "-XNoDeriveAnyClass"
     "-XNoDeriveDataTypeable"
     "-XNoDeriveFoldable"
     "-XNoDeriveFunctor"
     "-XNoDeriveGeneric"
     "-XNoDeriveLift"
     "-XNoDeriveTraversable"
     "-XNoDisambiguateRecordFields"
     "-XNoDoAndIfThenElse"
     "-XNoDoRec"
     "-XNoDuplicateRecordFields"
     "-XNoEmptyCase"
     "-XNoEmptyDataDecls"
     "-XNoExistentialQuantification"
     "-XNoExplicitForAll"
     "-XNoExplicitNamespaces"
     "-XNoExtendedDefaultRules"
     "-XNoFlexibleContexts"
     "-XNoFlexibleInstances"
     "-XNoForeignFunctionInterface"
     "-XNoFunctionalDependencies"
     "-XNoGADTSyntax"
     "-XNoGADTs"
     "-XNoGHCForeignImportPrim"
     "-XNoGeneralizedNewtypeDeriving"
     "-XNoGenerics"
     "-XNoImplicitParams"
     "-XNoImplicitPrelude"
     "-XNoImpredicativeTypes"
     "-XNoIncoherentInstances"
     "-XNoInstanceSigs"
     "-XNoInterruptibleFFI"
     "-XNoJavaScriptFFI"
     "-XNoKindSignatures"
     "-XNoLambdaCase"
     "-XNoLiberalTypeSynonyms"
     "-XNoMagicHash"
     "-XNoMonadComprehensions"
     "-XNoMonadFailDesugaring"
     "-XNoMonoLocalBinds"
     "-XNoMonoPatBinds"
     "-XNoMonomorphismRestriction"
     "-XNoMultiParamTypeClasses"
     "-XNoMultiWayIf"
     "-XNoNPlusKPatterns"
     "-XNoNamedFieldPuns"
     "-XNoNamedWildCards"
     "-XNoNegativeLiterals"
     "-XNoNondecreasingIndentation"
     "-XNoNullaryTypeClasses"
     "-XNoNumDecimals"
     "-XNoOverlappingInstances"
     "-XNoOverloadedLabels"
     "-XNoOverloadedLists"
     "-XNoOverloadedStrings"
     "-XNoPackageImports"
     "-XNoParallelArrays"
     "-XNoParallelListComp"
     "-XNoPartialTypeSignatures"
     "-XNoPatternGuards"
     "-XNoPatternSignatures"
     "-XNoPatternSynonyms"
     "-XNoPolyKinds"
     "-XNoPolymorphicComponents"
     "-XNoPostfixOperators"
     "-XNoQuasiQuotes"
     "-XNoRank2Types"
     "-XNoRankNTypes"
     "-XNoRebindableSyntax"
     "-XNoRecordPuns"
     "-XNoRecordWildCards"
     "-XNoRecursiveDo"
     "-XNoRelaxedLayout"
     "-XNoRelaxedPolyRec"
     "-XNoRoleAnnotations"
     "-XNoScopedTypeVariables"
     "-XNoStandaloneDeriving"
     "-XNoStaticPointers"
     "-XNoStrict"
     "-XNoStrictData"
     "-XNoTemplateHaskell"
     "-XNoTemplateHaskellQuotes"
     "-XNoTraditionalRecordSyntax"
     "-XNoTransformListComp"
     "-XNoTupleSections"
     "-XNoTypeApplications"
     "-XNoTypeFamilies"
     "-XNoTypeFamilyDependencies"
     "-XNoTypeInType"
     "-XNoTypeOperators"
     "-XNoTypeSynonymInstances"
     "-XNoUnboxedTuples"
     "-XNoUndecidableInstances"
     "-XNoUndecidableSuperClasses"
     "-XNoUnicodeSyntax"
     "-XNoUnliftedFFITypes"
     "-XNoViewPatterns"
     "-XNondecreasingIndentation"
     "-XNullaryTypeClasses"
     "-XNumDecimals"
     "-XOverlappingInstances"
     "-XOverloadedLabels"
     "-XOverloadedLists"
     "-XOverloadedStrings"
     "-XPackageImports"
     "-XParallelArrays"
     "-XParallelListComp"
     "-XPartialTypeSignatures"
     "-XPatternGuards"
     "-XPatternSignatures"
     "-XPatternSynonyms"
     "-XPolyKinds"
     "-XPolymorphicComponents"
     "-XPostfixOperators"
     "-XQuasiQuotes"
     "-XRank2Types"
     "-XRankNTypes"
     "-XRebindableSyntax"
     "-XRecordPuns"
     "-XRecordWildCards"
     "-XRecursiveDo"
     "-XRelaxedLayout"
     "-XRelaxedPolyRec"
     "-XRoleAnnotations"
     "-XSafe"
     "-XScopedTypeVariables"
     "-XStandaloneDeriving"
     "-XStaticPointers"
     "-XStrict"
     "-XStrictData"
     "-XTemplateHaskell"
     "-XTemplateHaskellQuotes"
     "-XTraditionalRecordSyntax"
     "-XTransformListComp"
     "-XTrustworthy"
     "-XTupleSections"
     "-XTypeApplications"
     "-XTypeFamilies"
     "-XTypeFamilyDependencies"
     "-XTypeInType"
     "-XTypeOperators"
     "-XTypeSynonymInstances"
     "-XUnboxedTuples"
     "-XUndecidableInstances"
     "-XUndecidableSuperClasses"
     "-XUnicodeSyntax"
     "-XUnliftedFFITypes"
     "-XUnsafe"
     "-XViewPatterns"
     "-auto"
     "-auto-all"
     "-c"
     "-caf-all"
     "-clear-package-db"
     "-cpp"
     "-dannot-lint"
     "-dasm-lint"
     "-dcmm-lint"
     "-dcore-lint"
     "-ddump-asm"
     "-ddump-asm-conflicts"
     "-ddump-asm-expanded"
     "-ddump-asm-liveness"
     "-ddump-asm-native"
     "-ddump-asm-regalloc"
     "-ddump-asm-regalloc-stages"
     "-ddump-asm-stats"
     "-ddump-bcos"
     "-ddump-call-arity"
     "-ddump-cmm"
     "-ddump-cmm-cbe"
     "-ddump-cmm-cfg"
     "-ddump-cmm-cps"
     "-ddump-cmm-info"
     "-ddump-cmm-proc"
     "-ddump-cmm-procmap"
     "-ddump-cmm-raw"
     "-ddump-cmm-sink"
     "-ddump-cmm-sp"
     "-ddump-cmm-split"
     "-ddump-cmm-switch"
     "-ddump-core-stats"
     "-ddump-cs-trace"
     "-ddump-cse"
     "-ddump-debug"
     "-ddump-deriv"
     "-ddump-ds"
     "-ddump-file-prefix"
     "-ddump-foreign"
     "-ddump-hi"
     "-ddump-hi-diffs"
     "-ddump-hpc"
     "-ddump-if-trace"
     "-ddump-inlinings"
     "-ddump-llvm"
     "-ddump-minimal-imports"
     "-ddump-mod-cycles"
     "-ddump-mod-map"
     "-ddump-occur-anal"
     "-ddump-opt-cmm"
     "-ddump-parsed"
     "-ddump-prep"
     "-ddump-rn"
     "-ddump-rn-stats"
     "-ddump-rn-trace"
     "-ddump-rtti"
     "-ddump-rule-firings"
     "-ddump-rule-rewrites"
     "-ddump-rules"
     "-ddump-simpl"
     "-ddump-simpl-iterations"
     "-ddump-simpl-stats"
     "-ddump-simpl-trace"
     "-ddump-spec"
     "-ddump-splices"
     "-ddump-stg"
     "-ddump-str-signatures"
     "-ddump-stranal"
     "-ddump-strsigs"
     "-ddump-tc"
     "-ddump-tc-trace"
     "-ddump-ticked"
     "-ddump-to-file"
     "-ddump-types"
     "-ddump-vect"
     "-ddump-view-pattern-commoning"
     "-ddump-vt-trace"
     "-ddump-worker-wrapper"
     "-debug"
     "-dep-makefile"
     "-dep-suffix"
     "-dfaststring-stats"
     "-dinitial-unique"
     "-distrust"
     "-distrust-all-packages"
     "-dno-debug-output"
     "-dno-llvm-mangler"
     "-dno-ppr-case-as-let"
     "-dno-ppr-ticks"
     "-dno-suppress-coercions"
     "-dno-suppress-idinfo"
     "-dno-suppress-module-prefixes"
     "-dno-suppress-type-applications"
     "-dno-suppress-type-signatures"
     "-dno-suppress-unfoldings"
     "-dno-suppress-uniques"
     "-dno-suppress-var-kinds"
     "-dppr-case-as-let"
     "-dppr-cols"
     "-dppr-debug"
     "-dppr-ticks"
     "-dppr-user-length"
     "-dshow-passes"
     "-dsource-stats"
     "-dstg-lint"
     "-dstg-stats"
     "-dsuppress-all"
     "-dsuppress-coercions"
     "-dsuppress-idinfo"
     "-dsuppress-module-prefixes"
     "-dsuppress-type-applications"
     "-dsuppress-type-signatures"
     "-dsuppress-unfoldings"
     "-dsuppress-uniques"
     "-dsuppress-var-kinds"
     "-dth-dec-file"
     "-dtrace-level"
     "-dumpdir"
     "-dunique-increment"
     "-dverbose-core2core"
     "-dverbose-stg2stg"
     "-dylib-install-name"
     "-dynamic"
     "-dynamic-too"
     "-dynhisuf"
     "-dynload"
     "-dyno"
     "-dynosuf"
     "-e"
     "-eventlog"
     "-exclude-module"
     "-fPArr"
     "-fPIC"
     "-fallow-incoherent-instances"
     "-fallow-overlapping-instances"
     "-fallow-undecidable-instances"
     "-farrows"
     "-fasm"
     "-fbang-patterns"
     "-fbuilding-cabal-package"
     "-fbyte-code"
     "-fcall-arity"
     "-fcase-merge"
     "-fcmm-elim-common-blocks"
     "-fcmm-sink"
     "-fconstraint-solver-iterations"
     "-fcontext-stack"
     "-fcpr-anal"
     "-fcpr-off"
     "-fcross-module-specialise"
     "-fcse"
     "-fdefer-type-errors"
     "-fdefer-typed-holes"
     "-fdicts-cheap"
     "-fdicts-strict"
     "-fdmd-tx-dict-sel"
     "-fdo-eta-reduction"
     "-fdo-lambda-eta-expansion"
     "-feager-blackholing"
     "-fembed-manifest"
     "-fenable-rewrite-rules"
     "-ferror-spans"
     "-fexcess-precision"
     "-fexpose-all-unfoldings"
     "-fext-core"
     "-fextended-default-rules"
     "-fexternal-interpreter"
     "-fffi"
     "-ffi"
     "-fflat-cache"
     "-ffloat-all-lams"
     "-ffloat-in"
     "-ffloat-lam-args"
     "-fforce-recomp"
     "-ffrontend-opt"
     "-ffull-laziness"
     "-ffun-to-thunk"
     "-fgen-manifest"
     "-fghci-history"
     "-fghci-sandbox"
     "-fglasgow-exts"
     "-fhelpful-errors"
     "-fhistory-size"
     "-fhpc"
     "-fhpc-no-auto"
     "-fignore-asserts"
     "-fignore-interface-pragmas"
     "-fimplicit-params"
     "-fimplicit-prelude"
     "-firrefutable-tuples"
     "-fkill-absence"
     "-fkill-one-shot"
     "-flate-dmd-anal"
     "-fliberate-case"
     "-fliberate-case-threshold"
     "-fllvm"
     "-floopification"
     "-fmax-inline-alloc-size"
     "-fmax-inline-memcpy-insns"
     "-fmax-inline-memset-insns"
     "-fmax-pmcheck-iterations"
     "-fmax-relevant-binds"
     "-fmax-simplifier-iterations"
     "-fmax-worker-args"
     "-fmono-pat-binds"
     "-fmonomorphism-restriction"
     "-fno-PArr"
     "-fno-PIC"
     "-fno-allow-incoherent-instances"
     "-fno-allow-overlapping-instances"
     "-fno-allow-undecidable-instances"
     "-fno-arrows"
     "-fno-bang-patterns"
     "-fno-building-cabal-package"
     "-fno-call-arity"
     "-fno-case-merge"
     "-fno-cmm-elim-common-blocks"
     "-fno-cmm-sink"
     "-fno-code"
     "-fno-cpr-anal"
     "-fno-cross-module-specialise"
     "-fno-cse"
     "-fno-defer-type-errors"
     "-fno-defer-typed-holes"
     "-fno-dicts-cheap"
     "-fno-dicts-strict"
     "-fno-dmd-tx-dict-sel"
     "-fno-do-eta-reduction"
     "-fno-do-lambda-eta-expansion"
     "-fno-eager-blackholing"
     "-fno-embed-manifest"
     "-fno-enable-rewrite-rules"
     "-fno-error-spans"
     "-fno-excess-precision"
     "-fno-expose-all-unfoldings"
     "-fno-ext-core"
     "-fno-extended-default-rules"
     "-fno-external-interpreter"
     "-fno-ffi"
     "-fno-fi"
     "-fno-flat-cache"
     "-fno-float-in"
     "-fno-force-recomp"
     "-fno-full-laziness"
     "-fno-fun-to-thunk"
     "-fno-gen-manifest"
     "-fno-ghci-history"
     "-fno-ghci-sandbox"
     "-fno-glasgow-exts"
     "-fno-helpful-errors"
     "-fno-hpc"
     "-fno-hpc-no-auto"
     "-fno-ignore-asserts"
     "-fno-ignore-interface-pragmas"
     "-fno-implicit-params"
     "-fno-implicit-prelude"
     "-fno-irrefutable-tuples"
     "-fno-kill-absence"
     "-fno-kill-one-shot"
     "-fno-late-dmd-anal"
     "-fno-liberate-case"
     "-fno-liberate-case-threshold"
     "-fno-loopification"
     "-fno-max-relevant-binds"
     "-fno-mono-pat-binds"
     "-fno-monomorphism-restriction"
     "-fno-omit-interface-pragmas"
     "-fno-omit-yields"
     "-fno-opt-coercion"
     "-fno-parr"
     "-fno-pedantic-bottoms"
     "-fno-pre-inlining"
     "-fno-print-equality-relations"
     "-fno-print-expanded-synonyms"
     "-fno-print-explicit-coercions"
     "-fno-print-explicit-foralls"
     "-fno-print-explicit-kinds"
     "-fno-print-explicit-runtime-reps"
     "-fno-print-potential-instances"
     "-fno-print-typechecker-elaboration"
     "-fno-print-unicode-syntax"
     "-fno-prof-auto"
     "-fno-prof-cafs"
     "-fno-prof-count-entries"
     "-fno-regs-graph"
     "-fno-regs-iterative"
     "-fno-reverse-errors"
     "-fno-rewrite-rules"
     "-fno-safe-infer"
     "-fno-scoped-type-variables"
     "-fno-shared-implib"
     "-fno-show-warning-groups"
     "-fno-simple-list-literals"
     "-fno-spec-constr"
     "-fno-spec-constr-count"
     "-fno-spec-constr-threshold"
     "-fno-specialise"
     "-fno-specialise-aggressively"
     "-fno-state-hack"
     "-fno-static-argument-transformation"
     "-fno-strictness"
     "-fno-th"
     "-fno-unbox-small-strict-fields"
     "-fno-unbox-strict-fields"
     "-fno-use-rpaths"
     "-fno-vectorisation-avoidance"
     "-fno-vectorise"
     "-fno-version-macros"
     "-fno-warn-"
     "-fno-warn-alternative-layout-rule-transitional"
     "-fno-warn-amp"
     "-fno-warn-auto-orphans"
     "-fno-warn-context-quantification"
     "-fno-warn-deprecated-flags"
     "-fno-warn-deprecations"
     "-fno-warn-deriving-typeable"
     "-fno-warn-dodgy-exports"
     "-fno-warn-dodgy-foreign-imports"
     "-fno-warn-dodgy-imports"
     "-fno-warn-duplicate-constraints"
     "-fno-warn-duplicate-exports"
     "-fno-warn-empty-enumerations"
     "-fno-warn-hi-shadowing"
     "-fno-warn-identities"
     "-fno-warn-implicit-prelude"
     "-fno-warn-incomplete-patterns"
     "-fno-warn-incomplete-record-updates"
     "-fno-warn-incomplete-uni-patterns"
     "-fno-warn-inline-rule-shadowing"
     "-fno-warn-missing-exported-sigs"
     "-fno-warn-missing-fields"
     "-fno-warn-missing-import-lists"
     "-fno-warn-missing-local-sigs"
     "-fno-warn-missing-methods"
     "-fno-warn-missing-signatures"
     "-fno-warn-monomorphism-restriction"
     "-fno-warn-name-shadowing"
     "-fno-warn-orphans"
     "-fno-warn-overflowed-literals"
     "-fno-warn-overlapping-patterns"
     "-fno-warn-partial-type-signatures"
     "-fno-warn-pointless-pragmas"
     "-fno-warn-safe"
     "-fno-warn-tabs"
     "-fno-warn-trustworthy-safe"
     "-fno-warn-type-defaults"
     "-fno-warn-typed-holes"
     "-fno-warn-unrecognised-pragmas"
     "-fno-warn-unsafe"
     "-fno-warn-unsupported-calling-conventions"
     "-fno-warn-unsupported-llvm-version"
     "-fno-warn-unticked-promoted-constructors"
     "-fno-warn-unused-binds"
     "-fno-warn-unused-do-bind"
     "-fno-warn-unused-imports"
     "-fno-warn-unused-matches"
     "-fno-warn-warnings-deprecations"
     "-fno-warn-wrong-do-bind"
     "-fno-worker-wrapper"
     "-fno-write-interface"
     "-fobject-code"
     "-fomit-interface-pragmas"
     "-fomit-yields"
     "-fpackage-trust"
     "-fparr"
     "-fpedantic-bottoms"
     "-fplugin"
     "-fplugin-opt"
     "-fpre-inlining"
     "-fprint-equality-relations"
     "-fprint-expanded-synonyms"
     "-fprint-explicit-coercions"
     "-fprint-explicit-foralls"
     "-fprint-explicit-kinds"
     "-fprint-explicit-runtime-reps"
     "-fprint-potential-instances"
     "-fprint-typechecker-elaboration"
     "-fprint-unicode-syntax"
     "-fprof-auto"
     "-fprof-auto-calls"
     "-fprof-auto-exported"
     "-fprof-auto-top"
     "-fprof-cafs"
     "-fprof-count-entries"
     "-framework"
     "-framework-path"
     "-freduction-depth"
     "-fregs-graph"
     "-fregs-iterative"
     "-freverse-errors"
     "-frewrite-rules"
     "-frule-check"
     "-fscoped-type-variables"
     "-fshared-implib"
     "-fshow-warning-groups"
     "-fsimpl-tick-factor"
     "-fsimple-list-literals"
     "-fsimplifier-phases"
     "-fspec-constr"
     "-fspec-constr-count"
     "-fspec-constr-recursive"
     "-fspec-constr-threshold"
     "-fspecialise"
     "-fspecialise-aggressively"
     "-fstatic-argument-transformation"
     "-fstrictness"
     "-fstrictness-before"
     "-fth"
     "-ftype-function-depth"
     "-funbox-small-strict-fields"
     "-funbox-strict-fields"
     "-funfolding-creation-threshold"
     "-funfolding-dict-discount"
     "-funfolding-fun-discount"
     "-funfolding-keeness-factor"
     "-funfolding-use-threshold"
     "-fuse-rpaths"
     "-fvectorisation-avoidance"
     "-fvectorise"
     "-fversion-macros"
     "-fvia-C"
     "-fvia-c"
     "-fwarn-"
     "-fwarn-alternative-layout-rule-transitional"
     "-fwarn-amp"
     "-fwarn-auto-orphans"
     "-fwarn-context-quantification"
     "-fwarn-deprecated-flags"
     "-fwarn-deprecations"
     "-fwarn-deriving-typeable"
     "-fwarn-dodgy-exports"
     "-fwarn-dodgy-foreign-imports"
     "-fwarn-dodgy-imports"
     "-fwarn-duplicate-constraints"
     "-fwarn-duplicate-exports"
     "-fwarn-empty-enumerations"
     "-fwarn-hi-shadowing"
     "-fwarn-identities"
     "-fwarn-implicit-prelude"
     "-fwarn-incomplete-patterns"
     "-fwarn-incomplete-record-updates"
     "-fwarn-incomplete-uni-patterns"
     "-fwarn-inline-rule-shadowing"
     "-fwarn-missing-exported-sigs"
     "-fwarn-missing-fields"
     "-fwarn-missing-import-lists"
     "-fwarn-missing-local-sigs"
     "-fwarn-missing-methods"
     "-fwarn-missing-signatures"
     "-fwarn-monomorphism-restriction"
     "-fwarn-name-shadowing"
     "-fwarn-orphans"
     "-fwarn-overflowed-literals"
     "-fwarn-overlapping-patterns"
     "-fwarn-partial-type-signatures"
     "-fwarn-pointless-pragmas"
     "-fwarn-safe"
     "-fwarn-tabs"
     "-fwarn-trustworthy-safe"
     "-fwarn-type-defaults"
     "-fwarn-typed-holes"
     "-fwarn-unrecognised-pragmas"
     "-fwarn-unsafe"
     "-fwarn-unsupported-calling-conventions"
     "-fwarn-unsupported-llvm-version"
     "-fwarn-unticked-promoted-constructors"
     "-fwarn-unused-binds"
     "-fwarn-unused-do-bind"
     "-fwarn-unused-imports"
     "-fwarn-unused-matches"
     "-fwarn-warnings-deprecations"
     "-fwarn-wrong-do-bind"
     "-fworker-wrapper"
     "-fwrite-interface"
     "-g"
     "-global-package-db"
     "-gransim"
     "-haddock"
     "-haddock-opts"
     "-hcsuf"
     "-hide-all-packages"
     "-hide-all-plugin-packages"
     "-hide-package"
     "-hidir"
     "-hisuf"
     "-hpcdir"
     "-i"
     "-ignore-package"
     "-include-pkg-deps"
     "-j"
     "-keep-hc-file"
     "-keep-hc-files"
     "-keep-llvm-file"
     "-keep-llvm-files"
     "-keep-s-file"
     "-keep-s-files"
     "-keep-tmp-files"
     "-l"
     "-main-is"
     "-mavx"
     "-mavx2"
     "-mavx512cd"
     "-mavx512er"
     "-mavx512f"
     "-mavx512pf"
     "-msse"
     "-msse2"
     "-msse3"
     "-msse4"
     "-msse4.2"
     "-n"
     "-ndp"
     "-no-auto"
     "-no-auto-all"
     "-no-auto-link-packages"
     "-no-caf-all"
     "-no-global-package-db"
     "-no-hs-main"
     "-no-link"
     "-no-recomp"
     "-no-rtsopts"
     "-no-rtsopts-suggestions"
     "-no-user-package-conf"
     "-no-user-package-db"
     "-o"
     "-odir"
     "-ohi"
     "-optF"
     "-optL"
     "-optP"
     "-opta"
     "-optc"
     "-opti"
     "-optl"
     "-optlc"
     "-optlo"
     "-optwindres"
     "-osuf"
     "-outputdir"
     "-package"
     "-package-conf"
     "-package-db"
     "-package-env"
     "-package-id"
     "-package-key"
     "-package-name"
     "-parallel"
     "-pgmF"
     "-pgmL"
     "-pgmP"
     "-pgma"
     "-pgmc"
     "-pgmdll"
     "-pgmi"
     "-pgml"
     "-pgmlc"
     "-pgmlibtool"
     "-pgmlo"
     "-pgms"
     "-pgmwindres"
     "-plugin-package"
     "-plugin-package-id"
     "-prof"
     "-rdynamic"
     "-recomp"
     "-relative-dynlib-paths"
     "-rtsopts"
     "-rtsopts=all"
     "-rtsopts=none"
     "-rtsopts=some"
     "-shared"
     "-sig-of"
     "-smp"
     "-split-objs"
     "-split-sections"
     "-static"
     "-staticlib"
     "-stubdir"
     "-syslib"
     "-this-package-key"
     "-this-unit-id"
     "-threaded"
     "-ticky"
     "-ticky-LNE"
     "-ticky-allocd"
     "-ticky-dyn-thunk"
     "-tmpdir"
     "-trust"
     "-user-package-db"
     "-v"
     "-w"
     "-with-rtsopts"
    )

  "GHC compiler options.

i.e. « -... »."

  :type '(list (string))

  :safe t

  :group 'sboo)

;;----------------------------------------------;;

  ;; (dolist (PRAGMA-CONS sboo-ghc-pragmas-alist)
  ;;   (pcase PRAGMA-CONS
  ;;     (`(,PRAGMA-STRING . ,READ-PRAGMA)
  ;;      )))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; =======================
;; `ghc' Pragmas
;; =======================

;; 1. LANGUAGE pragma
;; 2. OPTIONS_GHC pragma
;; 3. INCLUDE pragma
;; 4. WARNING and DEPRECATED pragmas
;; 5. MINIMAL pragma
;; 6. INLINE and NOINLINE pragmas
;; 6.1. INLINE pragma
;; 6.2. INLINABLE pragma
;; 6.3. NOINLINE pragma
;; 6.4. CONLIKE modifier
;; 6.5. Phase control
;; 7. LINE pragma
;; 8. COLUMN pragma
;; 9. RULES pragma
;; 10. SPECIALIZE pragma
;; 10.1. SPECIALIZE INLINE
;; 10.2. SPECIALIZE for imported functions
;; 10.3. Obsolete SPECIALIZE syntax
;; 11. SPECIALIZE instance pragma
;; 12. UNPACK pragma
;; 13. NOUNPACK pragma
;; 14. SOURCE pragma
;; 15. COMPLETE pragmas
;; 16. Disambiguating between multiple COMPLETE pragmas
;; 17. OVERLAPPING, OVERLAPPABLE, OVERLAPS, and INCOHERENT pragmas

;; See:
;;
;; - <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#pragmas>
;;

;;----------------------------------------------;;
(provide 'sboo-ghc)