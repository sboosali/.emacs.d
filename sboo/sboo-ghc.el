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
;; Types ---------------------------------------;;
;;----------------------------------------------;;

(cl-defstruct (sboo-haskell-symbol (:constructor sboo-haskell-symbol-create)
                                   (:copier nil))

  imports datatypes variables typeclasses instances)

;; ^ e.g.
;;
;;     M-: (sboo-haskell-symbol-create :imports '("") :datatypes '("") :variables '("") :typeclasses '("") :instances '(""))
;;

;;----------------------------------------------;;

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

(defun sboo-ghc-get-symbols-in-current-file ()

  "Names of all variables, datatypes, typeclasses, etc in the current buffer.

Output is a `plist' struct with these fields:

• Imports
• Datatypes
• Variables
• Typeclasses
• Instances

Output:

• a list of strings.

Related:

• `haskell-ds-create-imenu-index'."

  (progn
  
    ()))

;;----------------------------------------------;;

(defun sboo-ghc-get-variable-names-in-current-file ()
 
  "Get the names of all variables in the current buffer.

Output:

• a list of strings.

Related:

• `haskell-ds-create-imenu-index'."

  (progn
  
    ()))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-ghc-pragma-read ()

  "Read a GHC pragma, plus any of that pragma's required inputs.

Output:

• a list of strings.

Example:

• M-x sboo-ghc-pragma-read<RET>
      Pragma: LANGUAGE<RET>
      Language Extension: RecordWildCards<RET>
    ⇒ ()
  

Related:

• `sboo-ghc-pragmas-alist'.
• `sboo-ghc-pragma-read-pragma'."

  (interactive)

  (let ((PRAGMA-NAME (sboo-ghc-pragma-read-pragma))
        )

    (let* ((READ-PRAGMA-VALUES (alist-get PRAGMA-NAME sboo-ghc-pragmas-alist nil nil #'string-equal))
           (PRAGMA-VALUES      (funcall READ-PRAGMA-VALUES))
           (PRAGMA-LIST        (cons PRAGMA-NAME PRAGMA-VALUES))
           )

      PRAGMA-LIST)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-pragma ()

  "Read a GHC pragma.

Related:

• `sboo-ghc-pragmas-alist'."

  (interactive)

  (let ((prompt     "Pragma")
        (candidates sboo-ghc-pragmas-list)
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

    (completing-read (format "%s: " prompt) candidates)))

;;----------------------------------------------;;

(cl-defun sboo-ghc-read-string (&key prompt)

  "Read a haskell string.

Escapes:

• double-quotes (i.e. « \" »).

Related:

• `'."

  (interactive)

  (let ((PROMPT (or prompt "String"))
        (escape (if (require 'json nil :no-error)
                    #'json-encode-string
                  #'prin1-to-string))
        )

    (funcall escape
     (read-string (format "%s: " PROMPT)))))

;;----------------------------------------------;;

(cl-defun sboo-ghc-read-haskell-variable (&key prompt require-match)

  "Read a haskell variable.

Inputs:

• 

Output:

• 

Related:

• `'."

  (interactive)

  (let ((PROMPT        (format "%s: " (or prompt
                                          "Haskell Variable")))
        (REQUIRE-MATCH (or require-match
                           'confirm))
        (CANDIDATES    (sboo-ghc-get-variable-names-in-current-file))
        )

    (if CANDIDATES
        (completing-read PROMPT CANDIDATES nil REQUIRE-MATCH)

      (read-string PROMPT))))

;;----------------------------------------------;;

(defun sboo-ghc-read-haskell-variables ()

  "Read one-or-more haskell variables.

Output:

• a list of strings.

Related:

• `sboo-ghc-read-haskell-variable'."

  (interactive)

  (let ((PROMPT "Haskell Variable (empty to return the list)")
        (VARS   nil)
        (VAR    nil)
        )

    (while (not (string-empty-p
                 (setq VAR (sboo-ghc-read-haskell-variable :prompt        PROMPT
                                                           :require-match nil))))
      (push VAR VARS))

    (nreverse VARS)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-language-extension ()

  "Read a GHC language extension.

Related:

• `sboo-ghc-language-extensions'."

  (interactive)

  (let ((PROMPT     "Language Extension")
        (CANDIDATES sboo-ghc-language-extensions)
        )

    (completing-read (format "%s: " PROMPT)
                     CANDIDATES)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-compiler-option ()

  "Read a GHC compiler option.

Related:

• `sboo-ghc-compiler-options'."

  (interactive)

  (let ((PROMPT     "GHC Option")
        (CANDIDATES sboo-ghc-compiler-options)
        )

    (completing-read (format "%s: " PROMPT)
                     CANDIDATES)))

;;----------------------------------------------;;
;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-LANGUAGE ()

  "Returns `sboo-ghc-pragma-read-language-extension' as a list."

  (interactive)

  (list (sboo-ghc-pragma-read-language-extension)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-GHC_OPTION ()

  "Returns `sboo-ghc-pragma-read-compiler-option' as a list."

  (interactive)

  (list (sboo-ghc-pragma-read-compiler-option)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-WARNING (&optional )

  "Returns `sboo-ghc-read-string' as a list.

Examples (Haskell):

• « module Wobble {-# WARNING \"This is an unstable interface.\" #-} where »

• « {-# WARNING unsafePerformIO \"This is unsafe\" #-} »

Related:

• `sboo-ghc-read-DEPRECATED'."

  (interactive)

  (list (sboo-ghc-read-string)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-DEPRECATED ()

  "Returns `sboo-ghc-read-string' as a list.

Examples (Haskell):

• « module Wibble {-# DEPRECATED \"This is a legacy interface.\" #-} where »

• « {-# DEPRECATED f, C, T \"Don't use these\" #-} »

• « {-# DEPRECATED foo, bar [\"Don't use these\", \"Use gar instead\"] #-} »

Related:

• `sboo-ghc-read-WARNING'."

  (interactive)

  (list (sboo-ghc-read-string)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read- ()

  "Returns `sboo-ghc-read-string' as a list.

Examples (Haskell):

• «  »

Related:

• `'."

  (interactive)

  (list (sboo-ghc-read-string)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-INLINE ()

  "Returns `sboo-ghc-read-haskell-variable' as a list.

Examples (Haskell):

• «  »

Related:

• `'."

  (interactive)

  (list (sboo-ghc-read-haskell-variable)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-SPECIALIZE ()

  "Returns `sboo-ghc-read-haskell-binding' as a list.

Examples (Haskell):

• « {-# SPECIALIZE <variable> :: <type> #-} »

Related:

• `'."

  (interactive)

  (list (sboo-ghc-read-haskell-binding)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-RULES ()

  "Returns `sboo-ghc-read-string' as a list.

Examples (Haskell):

• « {-# RULES \"map/map\" forall f g xs.  map f (map g xs) = map (f.g) xs #-} »

Related:

• `'."

  (interactive)

  (list (sboo-ghc-read-string)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-OVERLAPPING ()

  "Returns `nil' as a list.

Examples (Haskell):

• « instance {-# OVERLAPPING #-} C t where ... »

Related:

• `'.

Links:

• URL `https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#overlapping-overlappable-overlaps-and-incoherent-pragmas'."

  (interactive)

  (list))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-COMPLETE ()

  "Returns `sboo-ghc-read-haskell-variables' as a list.

Output:

• Comma-Separated Strings.

Examples (Haskell):

• « {-# COMPLETE LeftChoice, RightChoice #-} »

Links:

• URL `https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#complete-pragmas'."

  (interactive)

  (let* ((VARIABLES (sboo-ghc-read-haskell-variables))
         (STRING (string-join VARIABLES ", "))
         )

  (list STRING)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-read-MINIMAL ()

  "Returns `sboo-ghc-read-' as a list.

Examples (Haskell):

• «  »

Related:

• `'."

  (interactive)

  (list (sboo-ghc-read-)))

;;----------------------------------------------;;
;;----------------------------------------------;;

(defun sboo-ghc-pragma-insert (&optional tokens)

  "Insert a GHC pragma comment, reading its name (and any mandatory values) if TOKENS is nil.

Inputs:

• TOKENS — a list of strings.

Related:

• `sboo-ghc-pragma-read-pragma'."

  (interactive (list
                (sboo-ghc-pragma-read)))

  (let* ((STRING (format "{-# %s #-}"
                         (string-join tokens " ")))
         )

    (insert STRING)))

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

(defun sboo-ghc-pragma-insert-COMPLETE (&optional variables)

  "Insert a « COMPLETE » pragma for VARIABLES.

Inputs:

• VARIABLES — a list of strings.

When invoked interactively, read a list of haskell variables
for VARIABLES (via `sboo-ghc-read-haskell-variables').

Related:

• `sboo-ghc-pragma-read-COMPLETE'."

  (interactive (list
                (sboo-ghc-read-haskell-variables)))

  (let* ((STRING-VARIABLES (string-join variables ", "))
         (STRING-PRAGMA    (format "{-# COMPLETE %s #-}" STRING-VARIABLES))
         )

    (insert STRING-PRAGMA)))

;;----------------------------------------------;;

(defun sboo-ghc-pragma-insert-UNPACK ()

  "Insert an « UNPACK » pragma."

  (interactive)

  (insert "{-# UNPACK #-}"))

;;----------------------------------------------;;
;; Aliases -------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;

(defcustom sboo-ghc-pragmas-alist ;TODO; make internal variable « hashtable ».

  '( ("LANGUAGE"            . sboo-ghc-pragma-read-LANGUAGE)

     ("OPTIONS_GHC"         . sboo-ghc-pragma-read-GHC_OPTION)

  ;; ("INCLUDE"             . sboo-ghc-pragma-read-INCLUDE)

     ("WARNING"             . sboo-ghc-pragma-read-WARNING)
     ("DEPRECATED"          . sboo-ghc-pragma-read-DEPRECATED)

     ("MINIMAL"             . sboo-ghc-pragma-read-MINIMAL)
     ("COMPLETE"            . sboo-ghc-pragma-read-COMPLETE)

     ("INLINABLE"           . sboo-ghc-pragma-read-INLINABLE)

     ("INLINE"              . sboo-ghc-pragma-read-INLINE)
     ("NOINLINE"            . sboo-ghc-pragma-read-INLINE)
     ("INLINE   CONLIKE"    . sboo-ghc-pragma-read-INLINE)
     ("NOINLINE CONLIKE"    . sboo-ghc-pragma-read-INLINE)

     ("RULES"               . sboo-ghc-pragma-read-RULES)

     ("SPECIALIZE"          . sboo-ghc-pragma-read-SPECIALIZE)
     ("SPECIALIZE INLINE"   . sboo-ghc-pragma-read-SPECIALIZE)
     ("SPECIALIZE NOINLINE" . sboo-ghc-pragma-read-SPECIALIZE)

     ("UNPACK"              . nil)
     ("NOUNPACK"            . nil)

     ("OVERLAPPING"         . sboo-ghc-pragma-read-OVERLAPPING)
     ("OVERLAPPABLE"        . sboo-ghc-pragma-read-OVERLAPPING)
     ("OVERLAPS"            . sboo-ghc-pragma-read-OVERLAPPING)
     ("INCOHERENT"          . sboo-ghc-pragma-read-OVERLAPPING)

     ;; ("LINE"             . sboo-ghc-pragma-read-LINE)
     ;; ("COLUMN"           . sboo-ghc-pragma-read-COLUMN)
     ;; ("SOURCE"           . sboo-ghc-pragma-read-SOURCE)
     )

  "GHC Pragmas and elisp commands (to read them from the user)."

  :type '(alist :key-type   (string :tag "Pragma")
                :value-type (choice (const nil)
                                    (function :tag "Completer/Reader"))
                )

  :safe t

  :group 'sboo-haskell)

;;----------------------------------------------;;

(defcustom sboo-ghc-pragmas-list

  (mapcar #'car
          sboo-ghc-pragmas-alist)

  "Known GHC Pragmas

i.e.  « {-# ... #-} »."

  :type '(list (string))

  :safe t

  :group 'sboo-haskell)

;;----------------------------------------------;;

(defcustom sboo-ghc-language-extensions

  '(
    "Haskell98"
    "Haskell2010"
    "Unsafe"
    "Trustworthy"
    "Safe"
    "AllowAmbiguousTypes"
    "NoAllowAmbiguousTypes"
    "AlternativeLayoutRule"
    "NoAlternativeLayoutRule"
    "AlternativeLayoutRuleTransitional"
    "NoAlternativeLayoutRuleTransitional"
    "Arrows"
    "NoArrows"
    "AutoDeriveTypeable"
    "NoAutoDeriveTypeable"
    "BangPatterns"
    "NoBangPatterns"
    "BinaryLiterals"
    "NoBinaryLiterals"
    "CApiFFI"
    "NoCApiFFI"
    "CPP"
    "NoCPP"
    "ConstrainedClassMethods"
    "NoConstrainedClassMethods"
    "ConstraintKinds"
    "NoConstraintKinds"
    "DataKinds"
    "NoDataKinds"
    "DatatypeContexts"
    "NoDatatypeContexts"
    "DefaultSignatures"
    "NoDefaultSignatures"
    "DeriveAnyClass"
    "NoDeriveAnyClass"
    "DeriveDataTypeable"
    "NoDeriveDataTypeable"
    "DeriveFoldable"
    "NoDeriveFoldable"
    "DeriveFunctor"
    "NoDeriveFunctor"
    "DeriveGeneric"
    "NoDeriveGeneric"
    "DeriveLift"
    "NoDeriveLift"
    "DeriveTraversable"
    "NoDeriveTraversable"
    "DerivingStrategies"
    "NoDerivingStrategies"
    "DerivingVia"
    "NoDerivingVia"
    "DisambiguateRecordFields"
    "NoDisambiguateRecordFields"
    "DoAndIfThenElse"
    "NoDoAndIfThenElse"
    "BlockArguments"
    "NoBlockArguments"
    "DoRec"
    "NoDoRec"
    "DuplicateRecordFields"
    "NoDuplicateRecordFields"
    "EmptyCase"
    "NoEmptyCase"
    "EmptyDataDecls"
    "NoEmptyDataDecls"
    "EmptyDataDeriving"
    "NoEmptyDataDeriving"
    "ExistentialQuantification"
    "NoExistentialQuantification"
    "ExplicitForAll"
    "NoExplicitForAll"
    "ExplicitNamespaces"
    "NoExplicitNamespaces"
    "ExtendedDefaultRules"
    "NoExtendedDefaultRules"
    "FlexibleContexts"
    "NoFlexibleContexts"
    "FlexibleInstances"
    "NoFlexibleInstances"
    "ForeignFunctionInterface"
    "NoForeignFunctionInterface"
    "FunctionalDependencies"
    "NoFunctionalDependencies"
    "GADTSyntax"
    "NoGADTSyntax"
    "GADTs"
    "NoGADTs"
    "GHCForeignImportPrim"
    "NoGHCForeignImportPrim"
    "GeneralizedNewtypeDeriving"
    "NoGeneralizedNewtypeDeriving"
    "GeneralisedNewtypeDeriving"
    "NoGeneralisedNewtypeDeriving"
    "ImplicitParams"
    "NoImplicitParams"
    "ImplicitPrelude"
    "NoImplicitPrelude"
    "ImpredicativeTypes"
    "NoImpredicativeTypes"
    "IncoherentInstances"
    "NoIncoherentInstances"
    "TypeFamilyDependencies"
    "NoTypeFamilyDependencies"
    "InstanceSigs"
    "NoInstanceSigs"
    "ApplicativeDo"
    "NoApplicativeDo"
    "InterruptibleFFI"
    "NoInterruptibleFFI"
    "JavaScriptFFI"
    "NoJavaScriptFFI"
    "KindSignatures"
    "NoKindSignatures"
    "LambdaCase"
    "NoLambdaCase"
    "LiberalTypeSynonyms"
    "NoLiberalTypeSynonyms"
    "MagicHash"
    "NoMagicHash"
    "MonadComprehensions"
    "NoMonadComprehensions"
    "MonadFailDesugaring"
    "NoMonadFailDesugaring"
    "MonoLocalBinds"
    "NoMonoLocalBinds"
    "MonoPatBinds"
    "NoMonoPatBinds"
    "MonomorphismRestriction"
    "NoMonomorphismRestriction"
    "MultiParamTypeClasses"
    "NoMultiParamTypeClasses"
    "MultiWayIf"
    "NoMultiWayIf"
    "NumericUnderscores"
    "NoNumericUnderscores"
    "NPlusKPatterns"
    "NoNPlusKPatterns"
    "NamedFieldPuns"
    "NoNamedFieldPuns"
    "NamedWildCards"
    "NoNamedWildCards"
    "NegativeLiterals"
    "NoNegativeLiterals"
    "HexFloatLiterals"
    "NoHexFloatLiterals"
    "NondecreasingIndentation"
    "NoNondecreasingIndentation"
    "NullaryTypeClasses"
    "NoNullaryTypeClasses"
    "NumDecimals"
    "NoNumDecimals"
    "OverlappingInstances"
    "NoOverlappingInstances"
    "OverloadedLabels"
    "NoOverloadedLabels"
    "OverloadedLists"
    "NoOverloadedLists"
    "OverloadedStrings"
    "NoOverloadedStrings"
    "PackageImports"
    "NoPackageImports"
    "ParallelArrays"
    "NoParallelArrays"
    "ParallelListComp"
    "NoParallelListComp"
    "PartialTypeSignatures"
    "NoPartialTypeSignatures"
    "PatternGuards"
    "NoPatternGuards"
    "PatternSignatures"
    "NoPatternSignatures"
    "PatternSynonyms"
    "NoPatternSynonyms"
    "PolyKinds"
    "NoPolyKinds"
    "PolymorphicComponents"
    "NoPolymorphicComponents"
    "QuantifiedConstraints"
    "NoQuantifiedConstraints"
    "PostfixOperators"
    "NoPostfixOperators"
    "QuasiQuotes"
    "NoQuasiQuotes"
    "Rank2Types"
    "NoRank2Types"
    "RankNTypes"
    "NoRankNTypes"
    "RebindableSyntax"
    "NoRebindableSyntax"
    "RecordPuns"
    "NoRecordPuns"
    "RecordWildCards"
    "NoRecordWildCards"
    "RecursiveDo"
    "NoRecursiveDo"
    "RelaxedLayout"
    "NoRelaxedLayout"
    "RelaxedPolyRec"
    "NoRelaxedPolyRec"
    "RoleAnnotations"
    "NoRoleAnnotations"
    "ScopedTypeVariables"
    "NoScopedTypeVariables"
    "StandaloneDeriving"
    "NoStandaloneDeriving"
    "StarIsType"
    "NoStarIsType"
    "StaticPointers"
    "NoStaticPointers"
    "Strict"
    "NoStrict"
    "StrictData"
    "NoStrictData"
    "TemplateHaskell"
    "NoTemplateHaskell"
    "TemplateHaskellQuotes"
    "NoTemplateHaskellQuotes"
    "TraditionalRecordSyntax"
    "NoTraditionalRecordSyntax"
    "TransformListComp"
    "NoTransformListComp"
    "TupleSections"
    "NoTupleSections"
    "TypeApplications"
    "NoTypeApplications"
    "TypeInType"
    "NoTypeInType"
    "TypeFamilies"
    "NoTypeFamilies"
    "TypeOperators"
    "NoTypeOperators"
    "TypeSynonymInstances"
    "NoTypeSynonymInstances"
    "UnboxedTuples"
    "NoUnboxedTuples"
    "UnboxedSums"
    "NoUnboxedSums"
    "UndecidableInstances"
    "NoUndecidableInstances"
    "UndecidableSuperClasses"
    "NoUndecidableSuperClasses"
    "UnicodeSyntax"
    "NoUnicodeSyntax"
    "UnliftedFFITypes"
    "NoUnliftedFFITypes"
    "ViewPatterns"
    "NoViewPatterns"
     )

  "Known GHC language extensions.

i.e. « -X... ».

Comes from « ghc --supported-extensions »."

  :type '(list (string))

  :safe t

  :group 'sboo-haskell)

;;----------------------------------------------;;

(defcustom sboo-ghc-compiler-options

  '(
    "-package-db"
    "-clear-package-db"
    "-no-global-package-db"
    "-no-user-package-db"
    "-global-package-db"
    "-user-package-db"
    "-package-conf"
    "-no-user-package-conf"
    "-package-name"
    "-this-package-key"
    "-this-unit-id"
    "-package"
    "-plugin-package-id"
    "-plugin-package"
    "-package-id"
    "-hide-package"
    "-hide-all-packages"
    "-hide-all-plugin-packages"
    "-package-env"
    "-ignore-package"
    "-syslib"
    "-distrust-all-packages"
    "-trust"
    "-distrust"
    "-n"
    "-cpp"
    "-F"
    "-#include"
    "-v"
    "-j"
    "-instantiated-with"
    "-this-component-id"
    "-H"
    "-Rghc-timing"
    "-prof"
    "-eventlog"
    "-smp"
    "-debug"
    "-threaded"
    "-ticky"
    "-static"
    "-dynamic"
    "-rdynamic"
    "-relative-dynlib-paths"
    "-copy-libs-when-linking"
    "-pie"
    "-no-pie"
    "-pgmlo"
    "-pgmlc"
    "-pgmi"
    "-pgmL"
    "-pgmP"
    "-pgmF"
    "-pgmc"
    "-pgms"
    "-pgma"
    "-pgml"
    "-pgmdll"
    "-pgmwindres"
    "-pgmlibtool"
    "-pgmar"
    "-pgmranlib"
    "-optlo"
    "-optlc"
    "-opti"
    "-optL"
    "-optP"
    "-optF"
    "-optc"
    "-opta"
    "-optl"
    "-optwindres"
    "-split-objs"
    "-split-sections"
    "-dep-suffix"
    "-dep-makefile"
    "-include-pkg-deps"
    "-exclude-module"
    "-no-link"
    "-shared"
    "-staticlib"
    "-dynload"
    "-dylib-install-name"
    "-L"
    "-l"
    "-framework-path"
    "-framework"
    "-odir"
    "-o"
    "-dyno"
    "-ohi"
    "-osuf"
    "-dynosuf"
    "-hcsuf"
    "-hisuf"
    "-dynhisuf"
    "-hidir"
    "-tmpdir"
    "-stubdir"
    "-dumpdir"
    "-outputdir"
    "-ddump-file-prefix"
    "-dynamic-too"
    "-keep-hc-file"
    "-keep-hc-files"
    "-keep-s-file"
    "-keep-s-files"
    "-keep-llvm-file"
    "-keep-llvm-files"
    "-keep-tmp-files"
    "-keep-hi-file"
    "-no-keep-hi-file"
    "-keep-hi-files"
    "-no-keep-hi-files"
    "-keep-o-file"
    "-no-keep-o-file"
    "-keep-o-files"
    "-no-keep-o-files"
    "-no-auto-link-packages"
    "-no-hs-main"
    "-fno-state-hack"
    "-fno-opt-coercion"
    "-with-rtsopts"
    "-rtsopts"
    "-rtsopts=all"
    "-rtsopts=some"
    "-rtsopts=none"
    "-rtsopts=ignore"
    "-rtsopts=ignoreAll"
    "-no-rtsopts"
    "-no-rtsopts-suggestions"
    "-dhex-word-literals"
    "-ghcversion-file"
    "-main-is"
    "-haddock"
    "-haddock-opts"
    "-hpcdir"
    "-ticky-allocd"
    "-ticky-LNE"
    "-ticky-dyn-thunk"
    "-recomp"
    "-no-recomp"
    "-fmax-errors"
    "-fno-max-errors"
    "-freverse-errors"
    "-fno-reverse-errors"
    "-D"
    "-U"
    "-I"
    "-i"
    "-dppr-user-length"
    "-dppr-cols"
    "-fdiagnostics-color=auto"
    "-fdiagnostics-color=always"
    "-fdiagnostics-color=never"
    "-dsuppress-all"
    "-dstg-stats"
    "-ddump-cmm"
    "-ddump-cmm-from-stg"
    "-ddump-cmm-raw"
    "-ddump-cmm-verbose"
    "-ddump-cmm-cfg"
    "-ddump-cmm-cbe"
    "-ddump-cmm-switch"
    "-ddump-cmm-proc"
    "-ddump-cmm-sp"
    "-ddump-cmm-sink"
    "-ddump-cmm-caf"
    "-ddump-cmm-procmap"
    "-ddump-cmm-split"
    "-ddump-cmm-info"
    "-ddump-cmm-cps"
    "-ddump-core-stats"
    "-ddump-asm"
    "-ddump-asm-native"
    "-ddump-asm-liveness"
    "-ddump-asm-regalloc"
    "-ddump-asm-conflicts"
    "-ddump-asm-regalloc-stages"
    "-ddump-asm-stats"
    "-ddump-asm-expanded"
    "-ddump-llvm"
    "-ddump-deriv"
    "-ddump-ds"
    "-ddump-ds-preopt"
    "-ddump-foreign"
    "-ddump-inlinings"
    "-ddump-rule-firings"
    "-ddump-rule-rewrites"
    "-ddump-simpl-trace"
    "-ddump-occur-anal"
    "-ddump-parsed"
    "-ddump-parsed-ast"
    "-ddump-rn"
    "-ddump-rn-ast"
    "-ddump-simpl"
    "-ddump-simpl-iterations"
    "-ddump-spec"
    "-ddump-prep"
    "-ddump-stg"
    "-ddump-call-arity"
    "-ddump-exitify"
    "-ddump-stranal"
    "-ddump-str-signatures"
    "-ddump-tc"
    "-ddump-tc-ast"
    "-ddump-types"
    "-ddump-rules"
    "-ddump-cse"
    "-ddump-worker-wrapper"
    "-ddump-rn-trace"
    "-ddump-shape"
    "-ddump-if-trace"
    "-ddump-cs-trace"
    "-ddump-tc-trace"
    "-ddump-ec-trace"
    "-ddump-vt-trace"
    "-ddump-splices"
    "-dth-dec-file"
    "-ddump-rn-stats"
    "-ddump-opt-cmm"
    "-ddump-simpl-stats"
    "-ddump-bcos"
    "-dsource-stats"
    "-dverbose-core2core"
    "-dverbose-stg2stg"
    "-ddump-hi"
    "-ddump-minimal-imports"
    "-ddump-hpc"
    "-ddump-ticked"
    "-ddump-mod-cycles"
    "-ddump-mod-map"
    "-ddump-timings"
    "-ddump-view-pattern-commoning"
    "-ddump-to-file"
    "-ddump-hi-diffs"
    "-ddump-rtti"
    "-dcore-lint"
    "-dstg-lint"
    "-dcmm-lint"
    "-dasm-lint"
    "-dannot-lint"
    "-dshow-passes"
    "-dfaststring-stats"
    "-dno-llvm-mangler"
    "-fast-llvm"
    "-ddump-debug"
    "-ddump-json"
    "-dppr-debug"
    "-ddebug-output"
    "-dno-debug-output"
    "-msse"
    "-msse2"
    "-msse3"
    "-msse4"
    "-msse4.2"
    "-mbmi"
    "-mbmi2"
    "-mavx"
    "-mavx2"
    "-mavx512cd"
    "-mavx512er"
    "-mavx512f"
    "-mavx512pf"
    "-W"
    "-Werror"
    "-Wwarn"
    "-Wnot"
    "-w"
    "-Weverything"
    "-Wno-everything"
    "-Wall"
    "-Wno-all"
    "-Wextra"
    "-Wno-extra"
    "-Wdefault"
    "-Wno-default"
    "-Wcompat"
    "-Wno-compat"
    "-fplugin-opt"
    "-fplugin"
    "-ffrontend-opt"
    "-Onot"
    "-O"
    "-fmax-relevant-binds"
    "-fno-max-relevant-binds"
    "-fmax-valid-hole-fits"
    "-fno-max-valid-hole-fits"
    "-fmax-refinement-hole-fits"
    "-fno-max-refinement-hole-fits"
    "-frefinement-level-hole-fits"
    "-fno-refinement-level-hole-fits"
    "-fmax-uncovered-patterns"
    "-fsimplifier-phases"
    "-fmax-simplifier-iterations"
    "-fmax-pmcheck-iterations"
    "-fsimpl-tick-factor"
    "-fspec-constr-threshold"
    "-fno-spec-constr-threshold"
    "-fspec-constr-count"
    "-fno-spec-constr-count"
    "-fspec-constr-recursive"
    "-fliberate-case-threshold"
    "-fno-liberate-case-threshold"
    "-drule-check"
    "-dinline-check"
    "-freduction-depth"
    "-fconstraint-solver-iterations"
    "-fcontext-stack"
    "-ftype-function-depth"
    "-fstrictness-before"
    "-ffloat-lam-args"
    "-ffloat-all-lams"
    "-fproc-alignment"
    "-fhistory-size"
    "-funfolding-creation-threshold"
    "-funfolding-use-threshold"
    "-funfolding-fun-discount"
    "-funfolding-dict-discount"
    "-funfolding-keeness-factor"
    "-fmax-worker-args"
    "-fmax-inline-alloc-size"
    "-fmax-inline-memcpy-insns"
    "-fmax-inline-memset-insns"
    "-dinitial-unique"
    "-dunique-increment"
    "-auto-all"
    "-no-auto-all"
    "-auto"
    "-no-auto"
    "-caf-all"
    "-no-caf-all"
    "-fprof-auto"
    "-fprof-auto-top"
    "-fprof-auto-exported"
    "-fprof-auto-calls"
    "-fno-prof-auto"
    "-fasm"
    "-fvia-c"
    "-fvia-C"
    "-fllvm"
    "-fno-code"
    "-fbyte-code"
    "-fobject-code"
    "-fglasgow-exts"
    "-fno-glasgow-exts"
    "-Wunused-binds"
    "-Wno-unused-binds"
    "-fpackage-trust"
    "-fno-safe-infer"
    "-fPIC"
    "-fno-PIC"
    "-fPIE"
    "-fno-PIE"
    "-g"
    "-dppr-case-as-let"
    "-dppr-ticks"
    "-dsuppress-ticks"
    "-dsuppress-stg-free-vars"
    "-dsuppress-coercions"
    "-dsuppress-idinfo"
    "-dsuppress-unfoldings"
    "-dsuppress-module-prefixes"
    "-dsuppress-timestamps"
    "-dsuppress-type-applications"
    "-dsuppress-type-signatures"
    "-dsuppress-uniques"
    "-dsuppress-var-kinds"
    "-dno-ppr-case-as-let"
    "-dno-ppr-ticks"
    "-dno-suppress-ticks"
    "-dno-suppress-stg-free-vars"
    "-dno-suppress-coercions"
    "-dno-suppress-idinfo"
    "-dno-suppress-unfoldings"
    "-dno-suppress-module-prefixes"
    "-dno-suppress-timestamps"
    "-dno-suppress-type-applications"
    "-dno-suppress-type-signatures"
    "-dno-suppress-uniques"
    "-dno-suppress-var-kinds"
    "-fasm-shortcutting"
    "-fbuilding-cabal-package"
    "-fcall-arity"
    "-fexitification"
    "-fcase-merge"
    "-fcase-folding"
    "-fcmm-elim-common-blocks"
    "-fcmm-sink"
    "-fcse"
    "-fstg-cse"
    "-fcpr-anal"
    "-fdefer-type-errors"
    "-fdefer-typed-holes"
    "-fdefer-out-of-scope-variables"
    "-fdiagnostics-show-caret"
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
    "-fexternal-dynamic-refs"
    "-fexternal-interpreter"
    "-fflat-cache"
    "-ffloat-in"
    "-fforce-recomp"
    "-fignore-optim-changes"
    "-fignore-hpc-changes"
    "-ffull-laziness"
    "-ffun-to-thunk"
    "-fgen-manifest"
    "-fghci-history"
    "-fghci-leak-check"
    "-fghci-sandbox"
    "-fhelpful-errors"
    "-fhpc"
    "-fignore-asserts"
    "-fignore-interface-pragmas"
    "-firrefutable-tuples"
    "-fkill-absence"
    "-fkill-one-shot"
    "-flate-dmd-anal"
    "-flate-specialise"
    "-fliberate-case"
    "-fllvm-pass-vectors-in-regs"
    "-floopification"
    "-fomit-interface-pragmas"
    "-fomit-yields"
    "-foptimal-applicative-do"
    "-fpedantic-bottoms"
    "-fpre-inlining"
    "-fprint-explicit-foralls"
    "-fprint-explicit-kinds"
    "-fprint-explicit-coercions"
    "-fprint-explicit-runtime-reps"
    "-fprint-equality-relations"
    "-fprint-unicode-syntax"
    "-fprint-expanded-synonyms"
    "-fprint-potential-instances"
    "-fprint-typechecker-elaboration"
    "-fprof-cafs"
    "-fprof-count-entries"
    "-fregs-graph"
    "-fregs-iterative"
    "-frewrite-rules"
    "-fshared-implib"
    "-fspec-constr"
    "-fspec-constr-keen"
    "-fspecialise"
    "-fspecialize"
    "-fspecialise-aggressively"
    "-fspecialize-aggressively"
    "-fcross-module-specialise"
    "-fcross-module-specialize"
    "-fstatic-argument-transformation"
    "-fstrictness"
    "-fuse-rpaths"
    "-fwrite-interface"
    "-funbox-small-strict-fields"
    "-funbox-strict-fields"
    "-fversion-macros"
    "-fworker-wrapper"
    "-fsolve-constant-dicts"
    "-fcatch-bottoms"
    "-falignment-sanitisation"
    "-fnum-constant-folding"
    "-fshow-warning-groups"
    "-fhide-source-paths"
    "-fshow-loaded-modules"
    "-fwhole-archive-hs-libs"
    "-fshow-hole-constraints"
    "-fshow-valid-substitutions"
    "-fshow-valid-hole-fits"
    "-fsort-valid-hole-fits"
    "-fsort-by-size-hole-fits"
    "-fsort-by-subsumption-hole-fits"
    "-fabstract-refinement-hole-fits"
    "-fshow-hole-matches-of-hole-fits"
    "-fshow-provenance-of-hole-fits"
    "-fshow-type-of-hole-fits"
    "-fshow-type-app-of-hole-fits"
    "-fshow-type-app-vars-of-hole-fits"
    "-funclutter-valid-hole-fits"
    "-fno-asm-shortcutting"
    "-fno-building-cabal-package"
    "-fno-call-arity"
    "-fno-exitification"
    "-fno-case-merge"
    "-fno-case-folding"
    "-fno-cmm-elim-common-blocks"
    "-fno-cmm-sink"
    "-fno-cse"
    "-fno-stg-cse"
    "-fno-cpr-anal"
    "-fno-defer-type-errors"
    "-fno-defer-typed-holes"
    "-fno-defer-out-of-scope-variables"
    "-fno-diagnostics-show-caret"
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
    "-fno-external-dynamic-refs"
    "-fno-external-interpreter"
    "-fno-flat-cache"
    "-fno-float-in"
    "-fno-force-recomp"
    "-fno-ignore-optim-changes"
    "-fno-ignore-hpc-changes"
    "-fno-full-laziness"
    "-fno-fun-to-thunk"
    "-fno-gen-manifest"
    "-fno-ghci-history"
    "-fno-ghci-leak-check"
    "-fno-ghci-sandbox"
    "-fno-helpful-errors"
    "-fno-hpc"
    "-fno-ignore-asserts"
    "-fno-ignore-interface-pragmas"
    "-fno-irrefutable-tuples"
    "-fno-kill-absence"
    "-fno-kill-one-shot"
    "-fno-late-dmd-anal"
    "-fno-late-specialise"
    "-fno-liberate-case"
    "-fno-llvm-pass-vectors-in-regs"
    "-fno-loopification"
    "-fno-omit-interface-pragmas"
    "-fno-omit-yields"
    "-fno-optimal-applicative-do"
    "-fno-pedantic-bottoms"
    "-fno-pre-inlining"
    "-fno-print-explicit-foralls"
    "-fno-print-explicit-kinds"
    "-fno-print-explicit-coercions"
    "-fno-print-explicit-runtime-reps"
    "-fno-print-equality-relations"
    "-fno-print-unicode-syntax"
    "-fno-print-expanded-synonyms"
    "-fno-print-potential-instances"
    "-fno-print-typechecker-elaboration"
    "-fno-prof-cafs"
    "-fno-prof-count-entries"
    "-fno-regs-graph"
    "-fno-regs-iterative"
    "-fno-rewrite-rules"
    "-fno-shared-implib"
    "-fno-spec-constr"
    "-fno-spec-constr-keen"
    "-fno-specialise"
    "-fno-specialize"
    "-fno-specialise-aggressively"
    "-fno-specialize-aggressively"
    "-fno-cross-module-specialise"
    "-fno-cross-module-specialize"
    "-fno-static-argument-transformation"
    "-fno-strictness"
    "-fno-use-rpaths"
    "-fno-write-interface"
    "-fno-unbox-small-strict-fields"
    "-fno-unbox-strict-fields"
    "-fno-version-macros"
    "-fno-worker-wrapper"
    "-fno-solve-constant-dicts"
    "-fno-catch-bottoms"
    "-fno-alignment-sanitisation"
    "-fno-num-constant-folding"
    "-fno-show-warning-groups"
    "-fno-hide-source-paths"
    "-fno-show-loaded-modules"
    "-fno-whole-archive-hs-libs"
    "-fno-show-hole-constraints"
    "-fno-show-valid-substitutions"
    "-fno-show-valid-hole-fits"
    "-fno-sort-valid-hole-fits"
    "-fno-sort-by-size-hole-fits"
    "-fno-sort-by-subsumption-hole-fits"
    "-fno-abstract-refinement-hole-fits"
    "-fno-show-hole-matches-of-hole-fits"
    "-fno-show-provenance-of-hole-fits"
    "-fno-show-type-of-hole-fits"
    "-fno-show-type-app-of-hole-fits"
    "-fno-show-type-app-vars-of-hole-fits"
    "-fno-unclutter-valid-hole-fits"
    "-Walternative-layout-rule-transitional"
    "-Wamp"
    "-Wauto-orphans"
    "-Wcpp-undef"
    "-Wunbanged-strict-patterns"
    "-Wdeferred-type-errors"
    "-Wdeferred-out-of-scope-variables"
    "-Wdeprecations"
    "-Wdeprecated-flags"
    "-Wderiving-typeable"
    "-Wdodgy-exports"
    "-Wdodgy-foreign-imports"
    "-Wdodgy-imports"
    "-Wempty-enumerations"
    "-Wduplicate-constraints"
    "-Wredundant-constraints"
    "-Wduplicate-exports"
    "-Whi-shadowing"
    "-Winaccessible-code"
    "-Wimplicit-prelude"
    "-Wimplicit-kind-vars"
    "-Wincomplete-patterns"
    "-Wincomplete-record-updates"
    "-Wincomplete-uni-patterns"
    "-Winline-rule-shadowing"
    "-Widentities"
    "-Wmissing-fields"
    "-Wmissing-import-lists"
    "-Wmissing-export-lists"
    "-Wmissing-local-sigs"
    "-Wmissing-local-signatures"
    "-Wmissing-methods"
    "-Wmissing-monadfail-instances"
    "-Wsemigroup"
    "-Wmissing-signatures"
    "-Wmissing-exported-sigs"
    "-Wmissing-exported-signatures"
    "-Wmonomorphism-restriction"
    "-Wname-shadowing"
    "-Wnoncanonical-monad-instances"
    "-Wnoncanonical-monadfail-instances"
    "-Wnoncanonical-monoid-instances"
    "-Worphans"
    "-Woverflowed-literals"
    "-Woverlapping-patterns"
    "-Wmissed-specialisations"
    "-Wmissed-specializations"
    "-Wall-missed-specialisations"
    "-Wall-missed-specializations"
    "-Wsafe"
    "-Wtrustworthy-safe"
    "-Wtabs"
    "-Wtype-defaults"
    "-Wtyped-holes"
    "-Wpartial-type-signatures"
    "-Wunrecognised-pragmas"
    "-Wunsafe"
    "-Wunsupported-calling-conventions"
    "-Wunsupported-llvm-version"
    "-Wunticked-promoted-constructors"
    "-Wunused-do-bind"
    "-Wunused-foralls"
    "-Wunused-imports"
    "-Wunused-local-binds"
    "-Wunused-matches"
    "-Wunused-pattern-binds"
    "-Wunused-top-binds"
    "-Wunused-type-patterns"
    "-Wwarnings-deprecations"
    "-Wwrong-do-bind"
    "-Wmissing-pattern-synonym-signatures"
    "-Wsimplifiable-class-constraints"
    "-Wmissing-home-modules"
    "-Wunrecognised-warning-flags"
    "-Wstar-binder"
    "-Wstar-is-type"
    "-Wpartial-fields"
    "-Wno-alternative-layout-rule-transitional"
    "-Wno-amp"
    "-Wno-auto-orphans"
    "-Wno-cpp-undef"
    "-Wno-unbanged-strict-patterns"
    "-Wno-deferred-type-errors"
    "-Wno-deferred-out-of-scope-variables"
    "-Wno-deprecations"
    "-Wno-deprecated-flags"
    "-Wno-deriving-typeable"
    "-Wno-dodgy-exports"
    "-Wno-dodgy-foreign-imports"
    "-Wno-dodgy-imports"
    "-Wno-empty-enumerations"
    "-Wno-duplicate-constraints"
    "-Wno-redundant-constraints"
    "-Wno-duplicate-exports"
    "-Wno-hi-shadowing"
    "-Wno-inaccessible-code"
    "-Wno-implicit-prelude"
    "-Wno-implicit-kind-vars"
    "-Wno-incomplete-patterns"
    "-Wno-incomplete-record-updates"
    "-Wno-incomplete-uni-patterns"
    "-Wno-inline-rule-shadowing"
    "-Wno-identities"
    "-Wno-missing-fields"
    "-Wno-missing-import-lists"
    "-Wno-missing-export-lists"
    "-Wno-missing-local-sigs"
    "-Wno-missing-local-signatures"
    "-Wno-missing-methods"
    "-Wno-missing-monadfail-instances"
    "-Wno-semigroup"
    "-Wno-missing-signatures"
    "-Wno-missing-exported-sigs"
    "-Wno-missing-exported-signatures"
    "-Wno-monomorphism-restriction"
    "-Wno-name-shadowing"
    "-Wno-noncanonical-monad-instances"
    "-Wno-noncanonical-monadfail-instances"
    "-Wno-noncanonical-monoid-instances"
    "-Wno-orphans"
    "-Wno-overflowed-literals"
    "-Wno-overlapping-patterns"
    "-Wno-missed-specialisations"
    "-Wno-missed-specializations"
    "-Wno-all-missed-specialisations"
    "-Wno-all-missed-specializations"
    "-Wno-safe"
    "-Wno-trustworthy-safe"
    "-Wno-tabs"
    "-Wno-type-defaults"
    "-Wno-typed-holes"
    "-Wno-partial-type-signatures"
    "-Wno-unrecognised-pragmas"
    "-Wno-unsafe"
    "-Wno-unsupported-calling-conventions"
    "-Wno-unsupported-llvm-version"
    "-Wno-unticked-promoted-constructors"
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
    "-Wno-missing-pattern-synonym-signatures"
    "-Wno-simplifiable-class-constraints"
    "-Wno-missing-home-modules"
    "-Wno-unrecognised-warning-flags"
    "-Wno-star-binder"
    "-Wno-star-is-type"
    "-Wno-partial-fields"
    "-Werror=alternative-layout-rule-transitional"
    "-Werror=amp"
    "-Werror=auto-orphans"
    "-Werror=cpp-undef"
    "-Werror=unbanged-strict-patterns"
    "-Werror=deferred-type-errors"
    "-Werror=deferred-out-of-scope-variables"
    "-Werror=deprecations"
    "-Werror=deprecated-flags"
    "-Werror=deriving-typeable"
    "-Werror=dodgy-exports"
    "-Werror=dodgy-foreign-imports"
    "-Werror=dodgy-imports"
    "-Werror=empty-enumerations"
    "-Werror=duplicate-constraints"
    "-Werror=redundant-constraints"
    "-Werror=duplicate-exports"
    "-Werror=hi-shadowing"
    "-Werror=inaccessible-code"
    "-Werror=implicit-prelude"
    "-Werror=implicit-kind-vars"
    "-Werror=incomplete-patterns"
    "-Werror=incomplete-record-updates"
    "-Werror=incomplete-uni-patterns"
    "-Werror=inline-rule-shadowing"
    "-Werror=identities"
    "-Werror=missing-fields"
    "-Werror=missing-import-lists"
    "-Werror=missing-export-lists"
    "-Werror=missing-local-sigs"
    "-Werror=missing-local-signatures"
    "-Werror=missing-methods"
    "-Werror=missing-monadfail-instances"
    "-Werror=semigroup"
    "-Werror=missing-signatures"
    "-Werror=missing-exported-sigs"
    "-Werror=missing-exported-signatures"
    "-Werror=monomorphism-restriction"
    "-Werror=name-shadowing"
    "-Werror=noncanonical-monad-instances"
    "-Werror=noncanonical-monadfail-instances"
    "-Werror=noncanonical-monoid-instances"
    "-Werror=orphans"
    "-Werror=overflowed-literals"
    "-Werror=overlapping-patterns"
    "-Werror=missed-specialisations"
    "-Werror=missed-specializations"
    "-Werror=all-missed-specialisations"
    "-Werror=all-missed-specializations"
    "-Werror=safe"
    "-Werror=trustworthy-safe"
    "-Werror=tabs"
    "-Werror=type-defaults"
    "-Werror=typed-holes"
    "-Werror=partial-type-signatures"
    "-Werror=unrecognised-pragmas"
    "-Werror=unsafe"
    "-Werror=unsupported-calling-conventions"
    "-Werror=unsupported-llvm-version"
    "-Werror=unticked-promoted-constructors"
    "-Werror=unused-do-bind"
    "-Werror=unused-foralls"
    "-Werror=unused-imports"
    "-Werror=unused-local-binds"
    "-Werror=unused-matches"
    "-Werror=unused-pattern-binds"
    "-Werror=unused-top-binds"
    "-Werror=unused-type-patterns"
    "-Werror=warnings-deprecations"
    "-Werror=wrong-do-bind"
    "-Werror=missing-pattern-synonym-signatures"
    "-Werror=simplifiable-class-constraints"
    "-Werror=missing-home-modules"
    "-Werror=unrecognised-warning-flags"
    "-Werror=star-binder"
    "-Werror=star-is-type"
    "-Werror=partial-fields"
    "-Wwarn=alternative-layout-rule-transitional"
    "-Wwarn=amp"
    "-Wwarn=auto-orphans"
    "-Wwarn=cpp-undef"
    "-Wwarn=unbanged-strict-patterns"
    "-Wwarn=deferred-type-errors"
    "-Wwarn=deferred-out-of-scope-variables"
    "-Wwarn=deprecations"
    "-Wwarn=deprecated-flags"
    "-Wwarn=deriving-typeable"
    "-Wwarn=dodgy-exports"
    "-Wwarn=dodgy-foreign-imports"
    "-Wwarn=dodgy-imports"
    "-Wwarn=empty-enumerations"
    "-Wwarn=duplicate-constraints"
    "-Wwarn=redundant-constraints"
    "-Wwarn=duplicate-exports"
    "-Wwarn=hi-shadowing"
    "-Wwarn=inaccessible-code"
    "-Wwarn=implicit-prelude"
    "-Wwarn=implicit-kind-vars"
    "-Wwarn=incomplete-patterns"
    "-Wwarn=incomplete-record-updates"
    "-Wwarn=incomplete-uni-patterns"
    "-Wwarn=inline-rule-shadowing"
    "-Wwarn=identities"
    "-Wwarn=missing-fields"
    "-Wwarn=missing-import-lists"
    "-Wwarn=missing-export-lists"
    "-Wwarn=missing-local-sigs"
    "-Wwarn=missing-local-signatures"
    "-Wwarn=missing-methods"
    "-Wwarn=missing-monadfail-instances"
    "-Wwarn=semigroup"
    "-Wwarn=missing-signatures"
    "-Wwarn=missing-exported-sigs"
    "-Wwarn=missing-exported-signatures"
    "-Wwarn=monomorphism-restriction"
    "-Wwarn=name-shadowing"
    "-Wwarn=noncanonical-monad-instances"
    "-Wwarn=noncanonical-monadfail-instances"
    "-Wwarn=noncanonical-monoid-instances"
    "-Wwarn=orphans"
    "-Wwarn=overflowed-literals"
    "-Wwarn=overlapping-patterns"
    "-Wwarn=missed-specialisations"
    "-Wwarn=missed-specializations"
    "-Wwarn=all-missed-specialisations"
    "-Wwarn=all-missed-specializations"
    "-Wwarn=safe"
    "-Wwarn=trustworthy-safe"
    "-Wwarn=tabs"
    "-Wwarn=type-defaults"
    "-Wwarn=typed-holes"
    "-Wwarn=partial-type-signatures"
    "-Wwarn=unrecognised-pragmas"
    "-Wwarn=unsafe"
    "-Wwarn=unsupported-calling-conventions"
    "-Wwarn=unsupported-llvm-version"
    "-Wwarn=unticked-promoted-constructors"
    "-Wwarn=unused-do-bind"
    "-Wwarn=unused-foralls"
    "-Wwarn=unused-imports"
    "-Wwarn=unused-local-binds"
    "-Wwarn=unused-matches"
    "-Wwarn=unused-pattern-binds"
    "-Wwarn=unused-top-binds"
    "-Wwarn=unused-type-patterns"
    "-Wwarn=warnings-deprecations"
    "-Wwarn=wrong-do-bind"
    "-Wwarn=missing-pattern-synonym-signatures"
    "-Wwarn=simplifiable-class-constraints"
    "-Wwarn=missing-home-modules"
    "-Wwarn=unrecognised-warning-flags"
    "-Wwarn=star-binder"
    "-Wwarn=star-is-type"
    "-Wwarn=partial-fields"
    "-Wno-error=alternative-layout-rule-transitional"
    "-Wno-error=amp"
    "-Wno-error=auto-orphans"
    "-Wno-error=cpp-undef"
    "-Wno-error=unbanged-strict-patterns"
    "-Wno-error=deferred-type-errors"
    "-Wno-error=deferred-out-of-scope-variables"
    "-Wno-error=deprecations"
    "-Wno-error=deprecated-flags"
    "-Wno-error=deriving-typeable"
    "-Wno-error=dodgy-exports"
    "-Wno-error=dodgy-foreign-imports"
    "-Wno-error=dodgy-imports"
    "-Wno-error=empty-enumerations"
    "-Wno-error=duplicate-constraints"
    "-Wno-error=redundant-constraints"
    "-Wno-error=duplicate-exports"
    "-Wno-error=hi-shadowing"
    "-Wno-error=inaccessible-code"
    "-Wno-error=implicit-prelude"
    "-Wno-error=implicit-kind-vars"
    "-Wno-error=incomplete-patterns"
    "-Wno-error=incomplete-record-updates"
    "-Wno-error=incomplete-uni-patterns"
    "-Wno-error=inline-rule-shadowing"
    "-Wno-error=identities"
    "-Wno-error=missing-fields"
    "-Wno-error=missing-import-lists"
    "-Wno-error=missing-export-lists"
    "-Wno-error=missing-local-sigs"
    "-Wno-error=missing-local-signatures"
    "-Wno-error=missing-methods"
    "-Wno-error=missing-monadfail-instances"
    "-Wno-error=semigroup"
    "-Wno-error=missing-signatures"
    "-Wno-error=missing-exported-sigs"
    "-Wno-error=missing-exported-signatures"
    "-Wno-error=monomorphism-restriction"
    "-Wno-error=name-shadowing"
    "-Wno-error=noncanonical-monad-instances"
    "-Wno-error=noncanonical-monadfail-instances"
    "-Wno-error=noncanonical-monoid-instances"
    "-Wno-error=orphans"
    "-Wno-error=overflowed-literals"
    "-Wno-error=overlapping-patterns"
    "-Wno-error=missed-specialisations"
    "-Wno-error=missed-specializations"
    "-Wno-error=all-missed-specialisations"
    "-Wno-error=all-missed-specializations"
    "-Wno-error=safe"
    "-Wno-error=trustworthy-safe"
    "-Wno-error=tabs"
    "-Wno-error=type-defaults"
    "-Wno-error=typed-holes"
    "-Wno-error=partial-type-signatures"
    "-Wno-error=unrecognised-pragmas"
    "-Wno-error=unsafe"
    "-Wno-error=unsupported-calling-conventions"
    "-Wno-error=unsupported-llvm-version"
    "-Wno-error=unticked-promoted-constructors"
    "-Wno-error=unused-do-bind"
    "-Wno-error=unused-foralls"
    "-Wno-error=unused-imports"
    "-Wno-error=unused-local-binds"
    "-Wno-error=unused-matches"
    "-Wno-error=unused-pattern-binds"
    "-Wno-error=unused-top-binds"
    "-Wno-error=unused-type-patterns"
    "-Wno-error=warnings-deprecations"
    "-Wno-error=wrong-do-bind"
    "-Wno-error=missing-pattern-synonym-signatures"
    "-Wno-error=simplifiable-class-constraints"
    "-Wno-error=missing-home-modules"
    "-Wno-error=unrecognised-warning-flags"
    "-Wno-error=star-binder"
    "-Wno-error=star-is-type"
    "-Wno-error=partial-fields"
    "-Werror=compat"
    "-Wno-error=compat"
    "-Wwarn=compat"
    "-fth"
    "-ffi"
    "-fffi"
    "-farrows"
    "-fimplicit-prelude"
    "-fbang-patterns"
    "-fmonomorphism-restriction"
    "-fmono-pat-binds"
    "-fextended-default-rules"
    "-fimplicit-params"
    "-fscoped-type-variables"
    "-fallow-overlapping-instances"
    "-fallow-undecidable-instances"
    "-fallow-incoherent-instances"
    "-fno-th"
    "-fno-fi"
    "-fno-ffi"
    "-fno-arrows"
    "-fno-implicit-prelude"
    "-fno-bang-patterns"
    "-fno-monomorphism-restriction"
    "-fno-mono-pat-binds"
    "-fno-extended-default-rules"
    "-fno-implicit-params"
    "-fno-scoped-type-variables"
    "-fno-allow-overlapping-instances"
    "-fno-allow-undecidable-instances"
    "-fno-allow-incoherent-instances"
    "-XAllowAmbiguousTypes"
    "-XAlternativeLayoutRule"
    "-XAlternativeLayoutRuleTransitional"
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
    "-XDerivingStrategies"
    "-XDerivingVia"
    "-XDisambiguateRecordFields"
    "-XDoAndIfThenElse"
    "-XBlockArguments"
    "-XDoRec"
    "-XDuplicateRecordFields"
    "-XEmptyCase"
    "-XEmptyDataDecls"
    "-XEmptyDataDeriving"
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
    "-XGeneralisedNewtypeDeriving"
    "-XImplicitParams"
    "-XImplicitPrelude"
    "-XImpredicativeTypes"
    "-XIncoherentInstances"
    "-XTypeFamilyDependencies"
    "-XInstanceSigs"
    "-XApplicativeDo"
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
    "-XNumericUnderscores"
    "-XNPlusKPatterns"
    "-XNamedFieldPuns"
    "-XNamedWildCards"
    "-XNegativeLiterals"
    "-XHexFloatLiterals"
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
    "-XQuantifiedConstraints"
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
    "-XScopedTypeVariables"
    "-XStandaloneDeriving"
    "-XStarIsType"
    "-XStaticPointers"
    "-XStrict"
    "-XStrictData"
    "-XTemplateHaskell"
    "-XTemplateHaskellQuotes"
    "-XTraditionalRecordSyntax"
    "-XTransformListComp"
    "-XTupleSections"
    "-XTypeApplications"
    "-XTypeInType"
    "-XTypeFamilies"
    "-XTypeOperators"
    "-XTypeSynonymInstances"
    "-XUnboxedTuples"
    "-XUnboxedSums"
    "-XUndecidableInstances"
    "-XUndecidableSuperClasses"
    "-XUnicodeSyntax"
    "-XUnliftedFFITypes"
    "-XViewPatterns"
    "-XNoAllowAmbiguousTypes"
    "-XNoAlternativeLayoutRule"
    "-XNoAlternativeLayoutRuleTransitional"
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
    "-XNoDerivingStrategies"
    "-XNoDerivingVia"
    "-XNoDisambiguateRecordFields"
    "-XNoDoAndIfThenElse"
    "-XNoBlockArguments"
    "-XNoDoRec"
    "-XNoDuplicateRecordFields"
    "-XNoEmptyCase"
    "-XNoEmptyDataDecls"
    "-XNoEmptyDataDeriving"
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
    "-XNoGeneralisedNewtypeDeriving"
    "-XNoImplicitParams"
    "-XNoImplicitPrelude"
    "-XNoImpredicativeTypes"
    "-XNoIncoherentInstances"
    "-XNoTypeFamilyDependencies"
    "-XNoInstanceSigs"
    "-XNoApplicativeDo"
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
    "-XNoNumericUnderscores"
    "-XNoNPlusKPatterns"
    "-XNoNamedFieldPuns"
    "-XNoNamedWildCards"
    "-XNoNegativeLiterals"
    "-XNoHexFloatLiterals"
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
    "-XNoQuantifiedConstraints"
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
    "-XNoStarIsType"
    "-XNoStaticPointers"
    "-XNoStrict"
    "-XNoStrictData"
    "-XNoTemplateHaskell"
    "-XNoTemplateHaskellQuotes"
    "-XNoTraditionalRecordSyntax"
    "-XNoTransformListComp"
    "-XNoTupleSections"
    "-XNoTypeApplications"
    "-XNoTypeInType"
    "-XNoTypeFamilies"
    "-XNoTypeOperators"
    "-XNoTypeSynonymInstances"
    "-XNoUnboxedTuples"
    "-XNoUnboxedSums"
    "-XNoUndecidableInstances"
    "-XNoUndecidableSuperClasses"
    "-XNoUnicodeSyntax"
    "-XNoUnliftedFFITypes"
    "-XNoViewPatterns"
    "-XHaskell98"
    "-XHaskell2010"
    "-XUnsafe"
    "-XTrustworthy"
    "-XSafe"
    "-XGenerics"
    "-XNoGenerics"
    "-?"
    "--help"
    "-V"
    "--version"
    "--numeric-version"
    "--info"
    "--show-options"
    "--supported-languages"
    "--supported-extensions"
    "--show-packages"
    "--print-project-version"
    "--print-project-git-commit-id"
    "--print-booter-version"
    "--print-stage"
    "--print-build-platform"
    "--print-host-platform"
    "--print-target-platform"
    "--print-have-interpreter"
    "--print-object-splitting-supported"
    "--print-have-native-code-generator"
    "--print-support-smp"
    "--print-unregisterised"
    "--print-tables-next-to-code"
    "--print-rts-ways"
    "--print-leading-underscore"
    "--print-debug-on"
    "--print-libdir"
    "--print-global-package-db"
    "--print-c-compiler-flags"
    "--print-c-compiler-link-flags"
    "--print-ld-flags"
    "--show-iface"
    "-c"
    "-M"
    "-E"
    "-C"
    "-S"
    "--make"
    "--backpack"
    "--interactive"
    "--abi-hash"
    "-e"
    "--frontend"
    )

  "Known GHC compiler options.

i.e. « -... ».

Comes from « ghc --show-options »."

  :type '(list (string))

  :safe t

  :group 'sboo-haskell)

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