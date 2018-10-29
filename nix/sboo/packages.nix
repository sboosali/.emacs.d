melpa: stable: elpa: org: 

############################################################
let
############################################################

criticalPackages = with melpa; [

 use-package     # minimal dependencies
 bind-key        #

 helm            # minimal dependencies
 helm-core       #
 async           #
 popup           #

 real-auto-save  # no dependencies
 
 haskell-mode    # no dependencies

 yasnippet       # no dependencies

];

############################################################

stablePackages = with stable; [

 ########################################
 # use-package + its (transitive) dependencies
 ########################################

 use-package
 bind-key
 use-package-el-get

 # https://stable.melpa.org/#/use-package
 # use-package-2.3
 # Dependencies: bind-key 1.0 / diminish 0.44

 ########################################
 # helm + its (transitive) dependencies
 ########################################

 helm
 helm-core
 async
 popup

 # https://stable.melpa.org/#/helm
 # helm-3.0
 # Dependencies: async 1.9.3 / emacs 24.4 / helm-core 3.0 / popup 0.5.3

 ########################################
 # utilities
 ########################################

 dash           # (the `-` prefix)
 s              # `s`trings
 f              # `f`iles

 # https://stable.melpa.org/#/f
 # Dependencies: dash 2.2.0 / s 1.7.0

 ########################################

 real-auto-save
 projectile

 #########################################

 company
 yasnippet

 #########################################

 haskell-mode
 dante           # needs `ghc` (or `cabal`, or `stack`, etc) system-package

 #########################################


 #########################################

];

############################################################

melpaPackages = with melpa; [

  #company-yasnippet
  #company-abbrev
  #company-dabbrev

  company-ghc
  company-ghci
  company-cabal
  company-web
  company-restclient
  company-anaconda

 ####################

 ghc
 dante

 ##intero          # needs `stack` system-package

 ####################

 anaconda-mode

 ####################

 tabbar
 shackle
 window-purpose

 ####################

 color-theme
 smooth-scrolling
 centered-cursor-mode

 ####################

 nix-mode
 paredit

 ####################

 magit          # <C-x g>

 direnv         # needs `direnv` system-package
 neotree
 wgrep

 multi-term

 markdown-mode 
 edit-indirect

 ####################

 json-mode
 yaml-mode

 restclient

 ####################

 helm-dash
 helm-make
 helm-swoop
 helm-hayoo

 ####################

 modalka
 # ^ https://github.com/mrkkrp/modalka/blob/master/README.md

 ##god-mode
 ##evil

 ########################

  anzu   
  # ^ shows total search hits in mode line.
  # c.f. `query-replace`.

  bm 
  # ^ visual bookmarks

  # csv-nav
  #  # ^ editing csv files

  deft
  # ^ quick note taking and management

  expand-region

  page-break-lines
  # ^ Convert the ^L (form feed) chars to horizontal lines

  wrap-region
  # ^ wrap selection with punctuations, tags (org-mode, markdown-mode, ..)

  yaml-mode

  #any-ini-mode # EmacsWiki only

 ########################

 epc
 elnode
 #emacs-web-server
 
 ########################
 # for haskell-ide-engine:

 lsp-mode
 lsp-ui 
 lsp-haskell

 ########################
 # from the "dired-hacks" megarepo:

 dired-filter
 dired-open
 dired-rainbow
 dired-subtree
 dired-ranger
 #dired-list
 dired-collapse

 ########################

 request
 # ^ https://tkf.github.io/emacs-request/manual.html

 ########################

 org.org
 # ^ 

 ########################

 wgrep
 # ^ https://github.com/mhayashi1120/Emacs-wgrep

 ########################

 # ^

];

############################################################

optionalPackages = with melpa; [

# helm-colors

 move-text
 # ^ 

 treemacs
 # ^ 

 tabbar-ruler
 # ^ 

 palimpsest
 # ^ 

 which-key
 # ^ https://github.com/justbur/emacs-which-key/blob/master/README.org

 elpa.xpm
 # ^ major-mode for editing XPM image files.
 # (is on ELPA)

 mediawiki
 # ^ major-mode for editing mediawiki-buffers.
 # (e.g. Wikipedia's markdown)

 # rainbow-mode
 # # ^ Color-ize Hex RGB Codes.
 # # (e.g. "#00ff00" looks green)

 volatile-highlights
 # ^ slightly shade recently-pasted region(s).

 goto-chg
 # ^ Go to the place where you last changed something.

 # ^

 # ^

];

############################################################

allPackages =
  melpaPackages ++ stablePackages ++ optionalPackages;

############################################################
in
############################################################

###stablePackages
###allPackages
###criticalPackages

allPackages

############################################################