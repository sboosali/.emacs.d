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

  "Major mode for editing Cabal files."

  ;;:link (url-link "")

  :prefix "cabal-"

  ;; :group 'languages
  :group 'haskell)

;;----------------------------------------------;;

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

    )

  "Known options of the « cabal new-build » subcommand.

Comes from « $ cabal new-build --list-options »."

  :type '(repeated (string :tag "Category"))

  :safe t
  :group 'cabal)

(1) ASP
(1) AST
(1) Accelerate
(1) Adjunctions
(1) Algorithm Visualization
(1) Algorithmic Music Composition
(1) Anatomy
(1) Applicative
(1) Arxiv
(1) Atom
(1) Attribute Grammars
(1) Automatic Music Generation
(1) BSD
(1) Base
(1) Binary
(1) Bits
(1) Bsparse
(1) Builders
(1) Bundle
(1) ByteStrings
(1) Bytes
(1) CAPTCHA
(1) CGI
(1) CI
(1) CPP
(1) Cabal
(1) Caching
(1) Cast
(1) Chaos Music
(1) Chat
(1) Code Competitions
(1) Coerce
(1) Combinatorics
(1) CommandLine
(1) Commerce
(1) Common-Parts
(1) Compilation
(1) Computer Algebra
(1) Constraint
(1) Contract
(1) Contracts
(1) Control.Monad
(1) Control.Parallel.Eden
(1) Convenience
(1) CouchDB
(1) Crosswords
(1) Cryptocurrency
(1) CsSyd
(1) Culinary
(1) Cursor
(1) DFINITY
(1) DNS
(1) Data Conduit
(1) Data Network
(1) Data-structures
(1) Database Design
(1) Decompiler
(1) Dependency Injection
(1) Derive-monoid
(1) DevOps
(1) Diagnostics
(1) Diagram
(1) EBNF
(1) ETL
(1) Editing
(1) Elm
(1) Emacs
(1) Envars
(1) Environment
(1) Eternity
(1) Eval.so
(1) Event-sourcing
(1) Eventloop
(1) Example
(1) Experiment
(1) Fake
(1) FakeData
(1) Feed
(1) FilePath
(1) Finance Network Bitcoin
(1) Fitness
(1) Flight
(1) Folding
(1) Fractals
(1) Futures
(1) GIS Programs
(1) GRPC
(1) Generative Music Grammars
(1) Gentoo
(1) Geospatial
(1) Gps
(1) Groundhog
(1) HAM
(1) HNum
(1) Hakyll
(1) Ham-radio
(1) Haskell2020
(1) Hxt
(1) Hydraulics
(1) Hydrology
(1) IRI
(1) Identification
(1) Indexed
(1) Interpolation
(1) Interpreter
(1) Interpreters
(1) IoT
(1) Iteratee
(1) JSX
(1) Japanese Natural Language Processing
(1) KML
(1) Kerf
(1) Ketchup
(1) Keyword Extractor
(1) LUA
(1) Lalr
(1) Lambda Cube
(1) Language Tools
(1) Lazy
(1) Learning Environments
(1) Learning Haskell
(1) Lexers
(1) LinearAlgebra
(1) Linux Desktop
(1) Lisp
(1) Little Game
(1) Local Search
(1) Logstash
(1) Macros
(1) Map
(1) MapReduce
(1) Matrix
(1) Memoization
(1) Memory
(1) Message-Oriented
(1) Meta
(1) Metalanguage
(1) Miscellaneous
(1) Miso
(1) MonadIO
(1) Mumeric.Statistics
(1) Murmur
(1) MusicBrainz
(1) NA
(1) Naqsha
(1) Natural Language
(1) Network Control
(1) NetworkAPI
(1) NetworkAPIs
(1) NonEmpty
(1) None
(1) Ntrol
(1) OAuth
(1) OCaml
(1) ODPI-C
(1) OOP
(1) Object Storage
(1) Ocilib
(1) Office
(1) OpenLayers
(1) Operations
(1) OverloadeLabels
(1) PDF Viewer
(1) PL/SQL Tools
(1) Package Management
(1) Packaging
(1) PagerDuty
(1) Parry
(1) Pattern Recognition
(1) PersonalGrowth
(1) Phishing
(1) Picture
(1) Planning
(1) Plotting
(1) Plugin
(1) Poker
(1) Pretty-printing
(1) Primitive
(1) Process Manager
(1) Program
(1) Prompt
(1) Proto
(1) Protocols
(1) Proxies
(1) Ptr
(1) PureScript
(1) QR
(1) RAKE
(1) RDF
(1) REPL
(1) RFC
(1) RSS/Atom Reader
(1) Raaz
(1) Radio
(1) Raw
(1) Redis
(1) Relational Algebra
(1) Relaxng
(1) Resources
(1) Rpc
(1) SCRIPT
(1) SDR
(1) SQL
(1) SQLite
(1) SVD
(1) Saas
(1) Scene
(1) Scientific
(1) Screencast
(1) Screensaver
(1) Selenium
(1) Semigroups
(1) Set Theory
(1) Si5351
(1) Silk
(1) Silly Tool
(1) Snaplet-fay
(1) Socket
(1) Sorting
(1) Source Tools
(1) Spreadsheet
(1) Steganography
(1) Stemming
(1) Stochastic Control
(1) Structures
(1) Subversion
(1) Support Vector Machine
(1) SyntComp
(1) Syslog
(1) Systems
(1) Tasty
(1) Tasty-kat
(1) TemplateHaskell
(1) Testing-hackage
(1) Text Editor
(1) Text Recognition
(1) Text.PrettyPrint
(1) Theorem Proving
(1) Time-frequency Distributions
(1) Timeout
(1) Tool
(1) Topology
(1) TouchDesigner
(1) Training
(1) Trans
(1) Translation
(1) Transpiler
(1) Tutorials
(1) Type Inference
(1) Type Theory
(1) Typechecking
(1) Typesystems
(1) URL
(1) Unikernel
(1) Unity3D
(1) User Interface
(1) Uzbl
(1) Validation
(1) Water
(1) Web Server
(1) Web Yesod
(1) Webframework
(1) Welcome
(1) Wiki
(1) Wsjtx
(1) X11
(1) XFCE
(1) YAML
(1) Yampa
(1) ZLib
(1) Zeromq
(1) Zip
(10) Authentication
(10) Bit Vectors
(10) Composition
(10) Email
(10) Extension
(10) Fay
(10) Git
(10) Haskell
(10) List
(10) Mathematics
(10) Media
(10) Reflection
(10) Symbolic Computation
(10) UI
(103) XML
(1084) Network
(11) Clckwrks
(11) Concurrent
(11) Utilities
(117) Algorithms
(118) Bioinformatics
(12) Eventsourcing
(12) Geometry
(12) IRC
(12) Monadic Regions
(12) Number Theory
(12) Optimisation
(12) Project
(12) SMT
(12) Search
(122) FFI
(129) Distribution
(13) Audio
(13) Compression
(13) HTML
(13) Networking
(13) Optimization
(13) Science
(13) Services
(13) Tensors
(13) Trace
(134) Cryptography
(138) Compilers/Interpreters
(14) Bitcoin
(14) Geography
(14) Java
(14) PostgreSQL
(144) Codec
(15) IDE
(15) Pretty Printer
(15) Protocol
(16) Benchmarking
(16) Comonads
(16) Effect
(16) Image
(16) JVM
(16) Network APIs
(16) Random
(1636) Web
(17) Combinators
(17) Constraints
(17) Filesystem
(17) Happstack
(17) Manatee
(17) Profiling
(17) Simulation
(175) AWS
(176) Google
(18) Desktop
(18) Nix
(18) Yi
(184) Sound
(186) Parsing
(1863) Data
(19) CSV
(194) Distributed Computing
(2) AOP
(2) Application Server
(2) Artificial Intelligence
(2) Aspect Oriented Programming
(2) Attoparsec
(2) Automation
(2) Backup
(2) Benchmark
(2) Big Data
(2) Blockchain
(2) Build Tool
(2) Building
(2) Cache
(2) Category
(2) Commercial
(2) Config
(2) Criu
(2) Data Flow
(2) Date
(2) Deep Learning
(2) Digest
(2) Download Manager
(2) Educational
(2) Exception
(2) Executable
(2) Experimental
(2) Factual
(2) Financial
(2) Foundation
(2) GPU
(2) Genealogy
(2) Geo
(2) GiveYouAHead
(2) Haskell98
(2) Help
(2) Heuristics
(2) I2C
(2) IRC Client
(2) Image Processing
(2) Interaction
(2) LambdaCalculus
(2) Lexer
(2) Lib
(2) Linear Algebra
(2) Linux
(2) Machine-learning
(2) Machines
(2) Medical
(2) Minecraft
(2) Multimedia Player
(2) Natural-language-processing
(2) Net
(2) Noise
(2) Numerics
(2) Opengl
(2) Oracle
(2) ParserCombinators
(2) Pattern
(2) Pattern Classification
(2) Persistent
(2) Politic
(2) Program Transformation
(2) Project Management
(2) Qux
(2) SIMD
(2) STM
(2) Sample Code
(2) Scientific Simulation
(2) Software Defined Radio
(2) Source Code Analysis
(2) Statistical Modeling
(2) System Tools
(2) TH
(2) Templating
(2) URI
(2) Unification
(2) Unsafe
(2) Ur/Web
(2) Video
(2) Visual Programming
(2) Visualization
(2) Vulkan
(2) Wai
(2) WebDriver
(2) Workflow
(2) Zippers
(20) Data Mining
(20) IO-Streams
(21) Editor
(21) JavaScript
(21) Monad
(212) Concurrency
(22) Aviation
(22) Parser
(23) CLI
(23) Documentation
(23) Error Handling
(23) GHC
(23) Serialization
(245) Data Structures
(25) Code Generation
(25) Crypto
(25) Failure
(25) Other
(25) Records
(251) Game
(26) Application
(26) Embedded
(28) Education
(29) Test
(3) Accessibility
(3) Apple
(3) Array
(3) ByteString
(3) CLI Tool
(3) Database Testing Web
(3) Databases
(3) Datamining
(3) Debugging
(3) Dhall
(3) Disassembler
(3) Eden
(3) Fedora
(3) File Manager
(3) Font
(3) Framework
(3) General
(3) Haskell2010
(3) Image Viewer
(3) Logic Programming
(3) Machine Vision
(3) Middleware
(3) Model
(3) Multimedia
(3) Mutable State
(3) Neovim
(3) Potoki
(3) Preprocessor
(3) Quantum
(3) QuickCheck
(3) RSS
(3) Raspberrypi
(3) Reverse Engineering
(3) Safe
(3) Scheduling
(3) Semantic Web
(3) Service
(3) Shell
(3) Svg
(3) TOML
(3) Transformation
(3) Unicode
(3) Unknown
(3) User-interface
(30) API
(30) Game Engine
(308) Testing
(31) Formal Methods
(311) Unclassified
(32) Library
(32) Physics
(32) Reactivity
(33) Configuration
(33) Enumerator
(33) Streaming
(34) Lenses
(34) Logic
(34) Security
(34) Snap
(344) Cloud
(36) Dependent Types
(36) Graphs
(36) Type System
(37) Servant
(38) Template Haskell
(38) Tools
(4) ADSB
(4) Algorithm
(4) Argumentation
(4) Avers
(4) C
(4) Charts
(4) Classification
(4) Command Line Tool
(4) Data Structure
(4) Desktop Environment
(4) GitHub
(4) Graph
(4) IO
(4) Languages
(4) Mail
(4) Maths
(4) Microcontroller
(4) N2O
(4) Operating System
(4) Polymorphism
(4) Reactive
(4) STM32
(4) Schema
(4) Software
(4) Stomp
(4) Stratux
(4) Succinct Data Structures
(4) Tree
(4) Types
(4) Uniform
(4) UserInterface
(4) Value
(40) Parallelism
(42) Bindings
(42) Debug
(42) Logging
(42) Time
(42) Utility
(425) Database
(43) Algebra
(44) Theorem Provers
(47) Pipes
(5) Arrows
(5) Bio
(5) Bsd3
(5) Cloud Haskell
(5) Command Line Tools
(5) Conversion
(5) Data Science
(5) Debian
(5) Functions
(5) Games
(5) Generic
(5) HTTP
(5) Linguistics
(5) Message-Oriented Middleware
(5) Mobile
(5) Parsers
(5) Password
(5) Phantom Types
(5) Pup-Events
(5) QuasiQuotes
(5) Refactoring
(5) Spam
(5) TODO
(5) Util
(5) XMonad
(51) ACME
(51) Statistics
(52) Numeric
(53) Compiler
(54) Hardware
(553) Graphics
(560) Math
(57) Finance
(57) Machine Learning
(590) Language
(6) Animation
(6) Bit
(6) Build
(6) CLR
(6) Client
(6) Compilers
(6) Containers
(6) DSL
(6) Distributed Systems
(6) Ethereum
(6) File
(6) Foreign Binding
(6) Hash
(6) LaTeX
(6) Money
(6) NLP
(6) Regex
(6) Scripting
(6) Simple
(6) String
(6) Swagger
(6) Syntax
(6) Typography
(62) Numerical
(66) Conduit
(67) AI
(68) GUI
(680) Development
(687) System
(7) Browser
(7) Chemistry
(7) Clustering
(7) Demo
(7) Exceptions
(7) Metrics
(7) Options
(7) Robotics
(7) Shake
(7) Static Analysis
(7) Teaching
(7) Vector
(7) Zift
(705) Control
(74) Prelude
(74) User Interfaces
(76) FRP
(79) Foreign
(79) JSON
(8) ATS
(8) Business
(8) Categories
(8) Codecs
(8) Command Line
(8) Compatibility
(8) Compiler Plugin
(8) Functors
(8) Interfaces
(8) Performance
(8) Pugs
(8) Recursion
(8) Source-tools
(8) Template
(8) Terminal
(8) Tonatona
(83) Console
(855) Text
(86) Utils
(88) Music
(89) Monads
(89) Natural Language Processing
(9) .NET
(9) FFI Tools
(9) Formal Languages
(9) Hasql
(9) Lens
(9) Monitoring
(9) PDF
(9) Validity
(98) Yesod
(99) Generics

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
        (candidates )
        )

    (completing-read (format "%s: " prompt) candidates)))

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