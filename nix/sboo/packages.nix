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

 exec-path-from-shell
 real-auto-save
 tabbar
 shackle
 window-purpose

 color-theme
 smooth-scrolling
 centered-cursor-mode

 yasnippet

 nix-mode
 paredit

 flycheck
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

 evil

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