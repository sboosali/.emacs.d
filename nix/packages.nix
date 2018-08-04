melpa: stable: elpa: org: 

############################################################
let

########################################

officialPackages = with melpa; [

 ####################
 # melpa...

 use-package

 dash           # (the `-` prefix)
 s              # `s`trings
 f              # `f`iles
 
 real-auto-save
 tabbar
 shackle
 window-purpose

 color-theme
 smooth-scrolling
 centered-cursor-mode

 flycheck
 multi-term

 haskell-mode
 dante           # needs `ghc` (or `cabal`, or `stack`, etc) system-package

 nix-mode

 markdown-mode 

 paredit

 magit          # <C-x g>
 projectile
 direnv         # needs `direnv` system-package

 helm
 helm-core
 helm-dash
 helm-make

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

];

########################################

unofficialPackages = [

];

in
############################################################

officialPackages ++ unofficialPackages

############################################################