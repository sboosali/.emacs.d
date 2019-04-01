;;; cabal.el --- Major mode for editing Cabal files -*- lexical-binding: t -*-

;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (haskell-mode "16.0") (cl-lib "0.5"))
;; Author: Spiros Boosalis <samboosalis@gmail.com> 
;; Maintainer: Spiros Boosalis <samboosalis@gmail.com> 
;; URL: https://github.com/sboosali/cabal-mode
;; Keywords: haskell, tools
;; Created: April 2019

;;----------------------------------------------;;

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2019 Sam Boosalis

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;==============================================;;
;;; Commentary:

;; Configuration for the `cabal' program (and format).
;;
;; See:
;;
;; * `cabal-*'
;; * `cabal-stanzas'
;; * `cabal-fields'
;; * `cabal-project-stanzas'
;; * `cabal-project-fields'
;;
;; Naming:
;;
;; * « cabal-* » — "Public" structs, macros, customizeables, commands, functions, and variables.
;; * « cabal/* » — "Private" functions and variables.
;;
;; 
;;

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(require 'cl-lib)
(require 'pcase)
(require 'rx)

;;

;;----------------------------------------------;;
;; Types ---------------------------------------;;
;;----------------------------------------------;;

(cl-defstruct (cabal-file (:constructor sboo-haskell-symbol-create)
                          (:copier nil))

  package)

;; ^ e.g.
;;
;;     M-: (sboo-haskell-symbol-create :package "")
;;

;;----------------------------------------------;;

(cl-defstruct (cabal-project-file (:constructor sboo-haskell-symbol-create)
                                  (:copier nil))

  packages)

;; ^ e.g.
;;
;;     M-: (sboo-haskell-symbol-create :packages '(""))
;;

;;----------------------------------------------;;
;; Macros --------------------------------------;;
;;----------------------------------------------;;

;;TODO rx

;;----------------------------------------------;;
;; Variables: Custom ---------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defgroup cabal nil

  "Cabal-related customization."

  ;;:link (url-link "")

  :prefix "cabal-"

  ;; :group 'languages
  :group 'haskell)

;;==============================================;;

(defcustom cabal-haskell-compilers

  '(ghc
    ghcjs
    eta
    (nhc . "nhc98")
    jhc
    lhc
    uhc
    yhc
    hbc
    hugs
    helium
    )

  "Haskell compilers, known to « cabal ».

Links:

• URL `https://github.com/haskell/cabal/blob/2.4/Cabal/Distribution/Compiler.hs'."

  :type '(repeated (choice (string :tag "Executable")
                           (cons (symbol :tag "Compiler")
                                 (string :tag "Executable"))))

  :safe #'listp
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-operating-systems

  '(
    Linux   ; tier 1 desktop OSs:
    Windows
    OSX

    FreeBSD ; other free Unix OSs:
    OpenBSD
    NetBSD
    DragonFly

    Solaris ; ageing Unix OSs:
    AIX
    HPUX
    IRIX

    HaLVM   ; bare metal / VMs / hypervisors:

    Hurd    ; GNU's microkernel:

    IOS     ; mobile OSs:
    Android

    Ghcjs
    )

  "Operating systems, known to « cabal ».

Links:

• URL `https://github.com/haskell/cabal/blob/2.4/Cabal/Distribution/System.hs'."

  :type '(repeated (symbol :tag "OS"))

  :safe #'listp
  :group 'cabal)

;;==============================================;;
;; Programs

(defcustom cabal-cabal-executable (executable-find "cabal")

  "Location of « cabal » executable."

  :type 'string

  :safe #'stringp
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-cabal2nix-executable (executable-find "cabal2nix")

  "Location of « cabal2nix » executable."

  :type 'string

  :safe #'stringp
  :group 'cabal)

;;==============================================;;
;; Programs (Haskell Compilers)

(defcustom cabal-ghc-executable (executable-find "ghc")

  "Location of « ghc » executable."

  :type 'string

  :safe #'stringp
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-ghc-pkg-executable (executable-find "ghc-pkg")

  "Location of « ghc-pkg » executable."

  :type 'string

  :safe #'stringp
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-ghcjs-executable (executable-find "ghcjs")

  "Location of « ghcjs » executable.

GHCJS compiles Haskell to JavaScript."

  :type 'string

  :safe #'stringp
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-eta-executable (executable-find "eta")

  "Location of « eta » executable.

ETA compiles Haskell onto JVM."

  :type 'string

  :safe #'stringp
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-nhc-executable (executable-find "nhc98")

  "Location of « nhc98 » executable."

  :type 'string

  :safe #'stringp
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-jhc-executable (executable-find "jhc")

  "Location of « jhc » executable."

  :type 'string

  :safe #'stringp
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-lhc-executable (executable-find "lhc")

  "Location of « lhc » executable."

  :type 'string

  :safe #'stringp
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-uhc-executable (executable-find "uhc")

  "Location of « uhc » executable."

  :type 'string

  :safe #'stringp
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-yhc-executable (executable-find "yhc")

  "Location of « yhc » executable."

  :type 'string

  :safe #'stringp
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-hbc-executable (executable-find "hbc")

  "Location of « hbc » executable."

  :type 'string

  :safe #'stringp
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-hugs-executable (executable-find "hugs")

  "Location of « hugs » executable."

  :type 'string

  :safe #'stringp
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-helium-executable (executable-find "helium")

  "Location of « helium » executable."

  :type 'string

  :safe #'stringp
  :group 'cabal)

;;==============================================;;

(defcustom cabal-fields-alist
                                        ;TODO; make internal variable « hashtable ». updated via « :modified ».

  '((name               . nil)
    (version            . nil)
    (cabal-version      . nil)
    (build-type         . nil)
    (license            . nil)
    (license-file       . nil)
    (license-files      . nil)
    (copyright          . nil)
    (author             . nil)
    (maintainer         . nil)
    (stability          . nil)
    (homepage           . nil)
    (bug-reports        . nil)
    (package-url        . nil)
    (synopsis           . nil)
    (description        . nil)
    (category           . nil)
    (tested-with        . nil)
    (data-files         . nil)
    (data-dir           . nil)
    (extra-source-files . nil)
    (extra-doc-files    . nil)
    (extra-tmp-files    . nil)
    )

  "Known fields in a cabal file, and their (elisp) commands.

Commands:

• completion / readers.
• parsers.
• validators."

  :type '(alist :key-type   (symbol :tag "Field")
                :value-type (choice (const nil)
                                    (function :tag "Command"))
                )

  :safe t
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-fields-list

  (mapcar #'symbol-name
          (mapcar #'car
                  cabal-fields-alist))

  "Known fields in a cabal file."

  :type '(repeated (string))

  :safe t
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-program-commands

  '(

    "list"
    "info"
    "fetch"
    "get"
    "check"
    "upload"
    "report"
    "init"
    "user-config"
    "gen-bounds"
    "outdated"
    "hscolour"
    "v2-configure"
    "new-configure"
    "v2-update"
    "new-update"
    "v2-build"
    "new-build"
    "v2-repl"
    "new-repl"
    "v2-freeze"
    "new-freeze"
    "v2-haddock"
    "new-haddock"
    "v2-install"
    "new-install"
    "v2-run"
    "new-run"
    "v2-test"
    "new-test"
    "v2-bench"
    "new-bench"
    "v2-exec"
    "new-exec"
    "v2-clean"
    "new-clean"
    "v2-sdist"
    "new-sdist"
    "configure"
    "v1-configure"
    "update"
    "v1-update"
    "build"
    "v1-build"
    "repl"
    "v1-repl"
    "freeze"
    "v1-freeze"
    "haddock"
    "v1-haddock"
    "install"
    "v1-install"
    "run"
    "v1-run"
    "test"
    "v1-test"
    "bench"
    "v1-bench"
    "exec"
    "v1-exec"
    "clean"
    "v1-clean"
    "sdist"
    "v1-sdist"
    "doctest"
    "v1-doctest"
    "copy"
    "v1-copy"
    "register"
    "v1-register"
    "reconfigure"
    "v1-reconfigure"
    "sandbox"
    "v1-sandbox"
    "help"

    )

  "Known (sub)commands of the « cabal » program.

Comes from « $ cabal --list-options », 
keeping only strings that start with an alphanumeric letter."

  :type '(repeated (string :tag "Command"))

  :safe t
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-program-options

  '(

    "-h"
    "--help"
    "-V"
    "--version"
    "--numeric-version"
    "--config-file"
    "--sandbox-config-file"
    "--default-user-config"
    "--require-sandbox"
    "--no-require-sandbox"
    "--ignore-sandbox"
    "--ignore-expiry"
    "--http-transport"
    "--enable-nix"
    "--disable-nix"
    
    )

  "Known options of the « cabal » program.

Comes from « $ cabal --list-options », 
keeping only strings that start with one-or-more hyphens."

  :type '(repeated (string :tag "Option"))

  :safe t
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-program-options

  '(

    "-h"
    "--help"
    "-v"
    "--verbose"
    "--builddir"
    "-g"
    "--ghc"
    "--ghcjs"
    "--uhc"
    "--haskell-suite"
    "--cabal-file"
    "-w"
    "--with-compiler"
    "--with-hc-pkg"
    "--prefix"
    "--bindir"
    "--libdir"
    "--libsubdir"
    "--dynlibdir"
    "--libexecdir"
    "--libexecsubdir"
    "--datadir"
    "--datasubdir"
    "--docdir"
    "--htmldir"
    "--haddockdir"
    "--sysconfdir"
    "--program-prefix"
    "--program-suffix"
    "--enable-library-vanilla"
    "--disable-library-vanilla"
    "-p"
    "--enable-library-profiling"
    "--disable-library-profiling"
    "--enable-shared"
    "--disable-shared"
    "--enable-static"
    "--disable-static"
    "--enable-executable-dynamic"
    "--disable-executable-dynamic"
    "--enable-profiling"
    "--disable-profiling"
    "--enable-executable-profiling"
    "--disable-executable-profiling"
    "--profiling-detail"
    "--library-profiling-detail"
    "-O"
    "--enable-optimization"
    "--enable-optimisation"
    "--disable-optimization"
    "--disable-optimisation"
    "--enable-debug-info"
    "--disable-debug-info"
    "--enable-library-for-ghci"
    "--disable-library-for-ghci"
    "--enable-split-sections"
    "--disable-split-sections"
    "--enable-split-objs"
    "--disable-split-objs"
    "--enable-executable-stripping"
    "--disable-executable-stripping"
    "--enable-library-stripping"
    "--disable-library-stripping"
    "--configure-option"
    "--user"
    "--global"
    "--package-db"
    "-f"
    "--flags"
    "--extra-include-dirs"
    "--enable-deterministic"
    "--disable-deterministic"
    "--ipid"
    "--cid"
    "--extra-lib-dirs"
    "--extra-framework-dirs"
    "--extra-prog-path"
    "--instantiate-with"
    "--enable-tests"
    "--disable-tests"
    "--enable-coverage"
    "--disable-coverage"
    "--enable-library-coverage"
    "--disable-library-coverage"
    "--enable-benchmarks"
    "--disable-benchmarks"
    "--enable-relocatable"
    "--disable-relocatable"
    "--disable-response-files"
    "--with-PROG"
    "--PROG-option"
    "--PROG-options"
    "--cabal-lib-version"
    "--constraint"
    "--preference"
    "--solver"
    "--allow-older"
    "--allow-newer"
    "--write-ghc-environment-files"
    "--enable-documentation"
    "--disable-documentation"
    "--doc-index-file"
    "--dry-run"
    "--max-backjumps"
    "--reorder-goals"
    "--count-conflicts"
    "--independent-goals"
    "--shadow-installed-packages"
    "--strong-flags"
    "--allow-boot-library-installs"
    "--reinstall"
    "--avoid-reinstalls"
    "--force-reinstalls"
    "--upgrade-dependencies"
    "--only-dependencies"
    "--dependencies-only"
    "--index-state"
    "--root-cmd"
    "--symlink-bindir"
    "--build-summary"
    "--build-log"
    "--remote-build-reporting"
    "--report-planning-failure"
    "--enable-per-component"
    "--disable-per-component"
    "--one-shot"
    "--run-tests"
    "-j"
    "--jobs"
    "--keep-going"
    "--offline"
    "--project-file"
    "--haddock-hoogle"
    "--haddock-html"
    "--haddock-html-location"
    "--haddock-for-hackage"
    "--haddock-executables"
    "--haddock-tests"
    "--haddock-benchmarks"
    "--haddock-all"
    "--haddock-internal"
    "--haddock-css"
    "--haddock-hyperlink-source"
    "--haddock-hyperlink-sources"
    "--haddock-hyperlinked-source"
    "--haddock-quickjump"
    "--haddock-hscolour-css"
    "--haddock-contents-location"
    "--only-configure"

    )

  "Known options of the « cabal new-build » subcommand.

Comes from « $ cabal new-build --list-options »."

  :type '(repeated (string :tag "Option"))

  :safe t
  :group 'cabal)

;;----------------------------------------------;;

(defcustom cabal-field-categories

  '(

    "Data"
    "Web"
    "Network"
    "Text"
    "Control"
    "System"
    "Development"
    "Language"
    "Math"
    "Graphics"
    "Database"
    "Cloud"
    "Unclassified"
    "Testing"
    "Game"
    "Data Structures"
    "Concurrency"
    "Distributed Computing"
    "Parsing"
    "Sound"
    "Google"
    "AWS"
    "Codec"
    "Compilers/Interpreters"
    "Cryptography"
    "Distribution"
    "FFI"
    "Bioinformatics"
    "Algorithms"
    "XML"
    "Generics"
    "Yesod"
    "Natural Language Processing"
    "Monads"
    "Music"
    "Utils"
    "Console"
    "JSON"
    "Foreign"
    "FRP"
    "User Interfaces"
    "Prelude"
    "GUI"
    "AI"
    "Conduit"
    "Numerical"
    "Machine Learning"
    "Finance"
    "Hardware"
    "Compiler"
    "Numeric"
    "Statistics"
    "ACME"
    "Pipes"
    "Theorem Provers"
    "Algebra"
    "Utility"
    "Time"
    "Logging"
    "Debug"
    "Bindings"
    "Parallelism"
    "Tools"
    "Template Haskell"
    "Servant"
    "Type System"
    "Graphs"
    "Dependent Types"
    "Snap"
    "Security"
    "Logic"
    "Lenses"
    "Streaming"
    "Enumerator"
    "Configuration"
    "Reactivity"
    "Physics"
    "Library"
    "Formal Methods"
    "Game Engine"
    "API"
    "Test"
    "Education"
    "Embedded"
    "Application"
    "Records"
    "Other"
    "Failure"
    "Crypto"
    "Code Generation"
    "Serialization"
    "GHC"
    "Error Handling"
    "Documentation"
    "CLI"
    "Parser"
    "Aviation"
    "Monad"
    "JavaScript"
    "Editor"
    "IO-Streams"
    "Data Mining"
    "CSV"
    "Yi"
    "Nix"
    "Desktop"
    "Simulation"
    "Profiling"
    "Manatee"
    "Happstack"
    "Filesystem"
    "Constraints"
    "Combinators"
    "Random"
    "Network APIs"
    "JVM"
    "Image"
    "Effect"
    "Comonads"
    "Benchmarking"
    "Protocol"
    "Pretty Printer"
    "IDE"
    "PostgreSQL"
    "Java"
    "Geography"
    "Bitcoin"
    "Trace"
    "Tensors"
    "Services"
    "Science"
    "Optimization"
    "Networking"
    "HTML"
    "Compression"
    "Audio"
    "Search"
    "SMT"
    "Project"
    "Optimisation"
    "Number Theory"
    "Monadic Regions"
    "IRC"
    "Geometry"
    "Eventsourcing"
    "Utilities"
    "Concurrent"
    "Clckwrks"
    "UI"
    "Symbolic Computation"
    "Reflection"
    "Media"
    "Mathematics"
    "List"
    "Haskell"
    "Git"
    "Fay"
    "Extension"
    "Email"
    "Composition"
    "Bit Vectors"
    "Authentication"
    "Validity"
    "PDF"
    "Monitoring"
    "Lens"
    "Hasql"
    "Formal Languages"
    "FFI Tools"
    ".NET"
    "Tonatona"
    "Terminal"
    "Template"
    "Source-tools"
    "Recursion"
    "Pugs"
    "Performance"
    "Interfaces"
    "Functors"
    "Compiler Plugin"
    "Compatibility"
    "Command Line"
    "Codecs"
    "Categories"
    "Business"
    "ATS"
    "Zift"
    "Vector"
    "Teaching"
    "Static Analysis"
    "Shake"
    "Robotics"
    "Options"
    "Metrics"
    "Exceptions"
    "Demo"
    "Clustering"
    "Chemistry"
    "Browser"
    "Typography"
    "Syntax"
    "Swagger"
    "String"
    "Simple"
    "Scripting"
    "Regex"
    "NLP"
    "Money"
    "LaTeX"
    "Hash"
    "Foreign Binding"
    "File"
    "Ethereum"
    "Distributed Systems"
    "DSL"
    "Containers"
    "Compilers"
    "Client"
    "CLR"
    "Build"
    "Bit"
    "Animation"
    "XMonad"
    "Util"
    "TODO"
    "Spam"
    "Refactoring"
    "QuasiQuotes"
    "Pup-Events"
    "Phantom Types"
    "Password"
    "Parsers"
    "Mobile"
    "Message-Oriented Middleware"
    "Linguistics"
    "HTTP"
    "Generic"
    "Games"
    "Functions"
    "Debian"
    "Data Science"
    "Conversion"
    "Command Line Tools"
    "Cloud Haskell"
    "Bsd3"
    "Bio"
    "Arrows"
    "Value"
    "UserInterface"
    "Uniform"
    "Types"
    "Tree"
    "Succinct Data Structures"
    "Stratux"
    "Stomp"
    "Software"
    "Schema"
    "STM32"
    "Reactive"
    "Polymorphism"
    "Operating System"
    "N2O"
    "Microcontroller"
    "Maths"
    "Mail"
    "Languages"
    "IO"
    "Graph"
    "GitHub"
    "Desktop Environment"
    "Data Structure"
    "Command Line Tool"
    "Classification"
    "Charts"
    "C"
    "Avers"
    "Argumentation"
    "Algorithm"
    "ADSB"
    "User-interface"
    "Unknown"
    "Unicode"
    "Transformation"
    "TOML"
    "Svg"
    "Shell"
    "Service"
    "Semantic Web"
    "Scheduling"
    "Safe"
    "Reverse Engineering"
    "Raspberrypi"
    "RSS"
    "QuickCheck"
    "Quantum"
    "Preprocessor"
    "Potoki"
    "Neovim"
    "Mutable State"
    "Multimedia"
    "Model"
    "Middleware"
    "Machine Vision"
    "Logic Programming"
    "Image Viewer"
    "Haskell2010"
    "General"
    "Framework"
    "Font"
    "File Manager"
    "Fedora"
    "Eden"
    "Disassembler"
    "Dhall"
    "Debugging"
    "Datamining"
    "Databases"
    "Database Testing Web"
    "CLI Tool"
    "ByteString"
    "Array"
    "Apple"
    "Accessibility"
    "Zippers"
    "Workflow"
    "WebDriver"
    "Wai"
    "Vulkan"
    "Visualization"
    "Visual Programming"
    "Video"
    "Ur/Web"
    "Unsafe"
    "Unification"
    "URI"
    "Templating"
    "TH"
    "System Tools"
    "Statistical Modeling"
    "Source Code Analysis"
    "Software Defined Radio"
    "Scientific Simulation"
    "Sample Code"
    "STM"
    "SIMD"
    "Qux"
    "Project Management"
    "Program Transformation"
    "Politic"
    "Persistent"
    "Pattern Classification"
    "Pattern"
    "ParserCombinators"
    "Oracle"
    "Opengl"
    "Numerics"
    "Noise"
    "Net"
    "Natural-language-processing"
    "Multimedia Player"
    "Minecraft"
    "Medical"
    "Machines"
    "Machine-learning"
    "Linux"
    "Linear Algebra"
    "Lib"
    "Lexer"
    "LambdaCalculus"
    "Interaction"
    "Image Processing"
    "IRC Client"
    "I2C"
    "Heuristics"
    "Help"
    "Haskell98"
    "GiveYouAHead"
    "Geo"
    "Genealogy"
    "GPU"
    "Foundation"
    "Financial"
    "Factual"
    "Experimental"
    "Executable"
    "Exception"
    "Educational"
    "Download Manager"
    "Digest"
    "Deep Learning"
    "Date"
    "Data Flow"
    "Criu"
    "Config"
    "Commercial"
    "Category"
    "Cache"
    "Building"
    "Build Tool"
    "Blockchain"
    "Big Data"
    "Benchmark"
    "Backup"
    "Automation"
    "Attoparsec"
    "Aspect Oriented Programming"
    "Artificial Intelligence"
    "Application Server"
    "AOP"

    )

  "Known categories of the « category: ... » cabal field.

Comes from downlading URL `http://hackage.haskell.org/packages/',
keeping any string that has been used at least twice."

  :type '(repeated (string :tag "Category"))

  :safe t
  :group 'cabal)

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(cl-defun cabal/alist->table (alist &key test size)

  "Create a Hash Table from ALIST.

Inputs:

• ALIST — an association `listp'. 
• TEST  — a `symbolp' (naming a `functionp'). See `make-hash-table'.
• SIZE  — an `integerp'. See `make-hash-table'.

Output:

• a `hash-table-p' with the same entries as ALIST (modulo duplicate keys).

Example:

• M-: (cabal/alist->table '((x . 1) (y . 2)) :test #'eq :size 3)
    ⇒ #s(hash-table size 3 test eq data (x 1 y 2))"

  (let* ((TEST  (or test
                    #'equal))

         (SIZE  (or size
                    (length alist)))

         (TABLE (make-hash-table :test TEST :size SIZE))

         )

    (progn

      (dolist (ENTRY alist)
        (let ((k (car ENTRY))
              (v (cdr ENTRY))
              )
          (puthash k v TABLE)))

      TABLE)))

;;----------------------------------------------;;

(cl-defun cabal-read-string (&key prompt)

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
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun cabal-insert-field-category (categories)

  "Insert one-or-more strings, for the « category » field.

Inputs:

• CATEGORIES — a `listp' of `stringp'.

Completion:

• `cabal-read-field-category'."

  (interactive (list (cabal-read-field-category)))

  (let ((STRING (string-trim (string-join categories ", ")))
        )

    (insert STRING)))

;;----------------------------------------------;;
;; Variables: "Private" ------------------------;;
;;----------------------------------------------;;

(defvar cabal-fields-table

  (mapcar #'car cabal-fields-alist)

  "Known fields in a cabal file."

  :type '(repeated (string))

  :safe t
  :group 'cabal)

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun cabal-read-field ()

  "Read a (known) cabal field, plus  that field's value.

Output:

• a `stringp'.

Example:

• M-x cabal-read-field<RET>
      Field: LANGUAGE<RET>
      Language Extension: RecordWildCards<RET>
    ⇒ ()
  

Related:

• `cabal-fields-alist'."

  (interactive)

  (let ((FIELD-NAME (cabal-field-read-field))
        )

    (let* ((READ-FIELD-VALUES (alist-get FIELD-NAME cabal-fields-alist nil nil #'string-equal))
           (FIELD-VALUES      (funcall READ-FIELD-VALUES))
           (FIELD-LIST        (cons FIELD-NAME FIELD-VALUES))
           )

      FIELD-LIST)))

;;----------------------------------------------;;

(defun cabal-read-stanza ()

  "Read a (known) cabal stanza.

Inputs:

• 

Output:

• a `stringp'.

Related:

• `cabal-stanzas-alist'."

  (interactive)

  (let ((prompt     "")
        (candidates ())
        )

    (completing-read (format "%s: " prompt) candidates)))

;;----------------------------------------------;;

(defun cabal-read-field-category ()

  "Read one-or-more strings, for the « category » field.

Output:

• a `listp' of `stringp'.

Completion:

• `cabal-field-categories'."

  (interactive)

  (let ((prompt     "Category")
        (candidates cabal-field-categories)
        )

    (completing-read (format "%s: " prompt) candidates)))

;; `read-

;;----------------------------------------------;;

(defun cabal-read-field-optimization ()

  "Read one-or-more strings, for the « category » field.

Output:

• an `integerp'.

Completion:

• `cabal-field-categories'."

  (interactive)

  (let ((PROMPT  "Optimization level:")
        (CHOICES '((?0 "0")
                   (?1 "1")
                   (?2 "2")
                   ))
        )

    (let* ((CHOSEN (read-multiple-choice PROMPT CHOICES))
           (LEVEL  (string-to-number (nth 1 CHOSEN)))
           )

      LEVEL)))

;; `read-multiple-choice':
;; 
;; (read-multiple-choice "Continue connecting?"
;;                       '((?a "always")
;;                         (?s "session only")
;;                         (?n "no")))
;;

;;----------------------------------------------;;

(defun cabal-mode-version ()

  "This feature's version.

Output:

• a `stringp'.

Example:

• M-: (version-to-list (cabal-mode-version))
    ⇒ '(0 1)

Related:

• wraps `pkg-info-version-info'"

  (require 'pkg-info)

  (pkg-info-version-info 'cabal))

;;----------------------------------------------;;
;; Keymaps -------------------------------------;;
;;----------------------------------------------;;

;; Keyboard:

;;----------------------------------------------;;

;; Menubar:

;; (defvar cabal-map nil
;;   "Local keymap for cabal buffers.")
;;
;; (if cabal-map
;;   nil
;;   (let ((map (make-keymap)))
;;     (define-key map "\^c\^c" 'cabal-read-current-file)
;;     (define-key map "\^c\^h" 'cabal-man)
;;     (define-key map "\^c\^s" 'cabal-shell-command)
;;     (easy-menu-define
;;       cabal-easy-menu map
;;       "Menu for Cabal mode."
;;       '("Cabal"
;; 	 ["Read current file" cabal-read-current-file t]
;; 	 ["Start cabal command" cabal-shell-command t]
;; 	 ["-" nil t]
;; 	 ["Cabal manual page" cabal-man t]))
;;     (setq cabal-map map)))

;;----------------------------------------------;;

;; Toolbar:

;;----------------------------------------------;;
;; Mode ----------------------------------------;;
;;----------------------------------------------;;

;; cabal-mode "Edit « .cabal » files."

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (rx "." "cabal" eos) #'cabal-mode))

;;----------------------------------------------;;
;; Aliases -------------------------------------;;
;;----------------------------------------------;;

(defalias 'cabal-mode-version #'pkg-info-version-info)

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; =======================
;; `*.cabal' stanzas
;; =======================

;; =======================
;; `*.cabal' fields
;; =======================

;; - Package properties:
;;
;;     name
;;     version
;;     cabal-version
;;     build-type
;;     license
;;     license-file
;;     license-files
;;     copyright
;;     author
;;     maintainer
;;     stability
;;     homepage
;;     bug-reports
;;     package-url
;;     synopsis
;;     description
;;     category
;;     tested-with
;;     data-files
;;     data-dir
;;     extra-source-files
;;     extra-doc-files
;;     extra-tmp-files

;; - Library:
;;
;;     library — Library build information.
;;     exposed-modules
;;     virtual-modules (since version: 2.2)
;;     exposed
;;     reexported-modules (since version: 1.22)
;;     signatures (since version: 2.0)

;; - Executables:
;;
;;     executable — Executable build info section.
;;     main-is
;;     scope (since version: 2.0)

;; - Test suites:
;;
;;     test-suite — Test suite build information.
;;     type
;;     main-isModule containing tests main function.
;;     test-module

;; - Benchmarks:
;;
;;     benchmark (since version: 1.9.2) — Benchmark build information.
;;     type
;;     main-is

;; - Foreign libraries:
;;
;;     foreign-library (since version: 2.0) — Foriegn library build information.
;;     type
;;     options
;;     mod-def-file
;;     lib-version-info
;;     lib-version-linux

;; - Build information:
;;
;;     build-depends
;;     other-modules
;;     hs-source-dirs
;;     default-extensions
;;     other-extensions
;;     extensions (deprecated)
;;     build-tool-depends (since version: 2.0)
;;     build-tools (deprecated)
;;     buildable
;;     ghc-options
;;     ghc-prof-options
;;     ghc-shared-options
;;     includes
;;     install-includes
;;     include-dirs
;;     c-sources
;;     cxx-sources (since version: 2.2)
;;     asm-sources
;;     cmm-sources
;;     js-sources
;;     extra-libraries
;;     extra-ghci-libraries
;;     extra-bundled-libraries
;;     extra-lib-dirs
;;     cc-options
;;     cpp-options
;;     cxx-options (since version: 2.2)
;;     ld-options
;;     pkgconfig-depends
;;     frameworks
;;     extra-frameworks-dirs
;;     mixins (since version: 2.0)

;; - Configuration Flags:
;;
;;     flagFlag declaration.
;;     description
;;     default
;;     manual

;; - Common stanzas:
;;
;;     common (since version: 2.2) — Common build info section

;; - Source Repositories:
;;
;;     source-repository (since version: 1.6)
;;     type
;;     location
;;     module
;;     branch
;;     tag
;;     subdir

;; - Custom setup scripts:
;;
;;     custom-setup (since version: 1.24) — Custom Setup.hs build information.
;;     setup-depends (since version: 1.24)

;; - Autogenerated modules:
;;
;;     autogen-modules (since version: 2.0)

;; =======================
;; `cabal.project' stanzas
;; =======================

;; =======================
;; `cabal.project' fields
;; =======================

;;----------------------------------------------;;

;; See:
;;
;; • <https://cabal.readthedocs.io/en/latest/developing-packages.html#developing-packages>
;; • <https://cabal.readthedocs.io/en/latest/nix-local-build.html>
;; • <https://www.haskell.org/cabal/users-guide/cabal-projectindex.html>
;; • <https://melpa.org/#/cabal>
;;

;;==============================================;;
(provide 'cabal)

;;----------------------------------------------;;

;; Local Variables:
;; End:

;;; `cabal.el` ends here