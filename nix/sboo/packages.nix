melpa: stable: elpa: org: 

############################################################
let

########################################

officialPackages = with melpa; [

 ####################

 use-package
 bind-key

 ####################

 dash           # (the `-` prefix)
 s              # `s`trings
 f              # `f`iles
 
 ####################

 haskell-mode
 dante           # needs `ghc` (or `cabal`, or `stack`, etc) system-package
 intero          # needs `stack` system-package

 ####################

 lsp-mode
 lsp-ui
 lsp-haskell

 ####################

 flycheck
 flycheck-haskell

 ####################

 exec-path-from-shell
 real-auto-save

 ####################

 tabbar
 shackle
 window-purpose

 ####################

 color-theme
 smooth-scrolling
 centered-cursor-mode

 ####################

 yasnippet

 ####################

 nix-mode
 paredit

 ####################

 #magit          # <C-x g>
 projectile
 direnv         # needs `direnv` system-package
 #multi-term
 neotree
 wgrep

 markdown-mode 
 edit-indirect

 json-mode
 yaml-mode

 ####################

 ####################

 helm
 helm-core
 helm-dash
 helm-make
 helm-swoop

 ########################

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

 move-text
 treemacs
 tabbar-ruler

 ########################

 request
 # ^ https://tkf.github.io/emacs-request/manual.html

 ########################

 wgrep
 # ^ https://github.com/mhayashi1120/Emacs-wgrep

 ########################

 org.org

 ########################

 palimpsest

 ########################

 which-key
 # ^ https://github.com/justbur/emacs-which-key/blob/master/README.org

 ########################

];

########################################

unofficialPackages = [

];

########################################

optionalPackages = with melpa; [

# helm-colors

];

in
############################################################

officialPackages ++ unofficialPackages ++ optionalPackages

############################################################