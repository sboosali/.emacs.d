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

 exec-path-from-shell
 real-auto-save
 tabbar
 shackle
 window-purpose

 color-theme
 smooth-scrolling
 centered-cursor-mode

 yasnippet

 flycheck
 magit          # <C-x g>
 projectile
 direnv         # needs `direnv` system-package
 #multi-term

 haskell-mode
 dante           # needs `ghc` (or `cabal`, or `stack`, etc) system-package
 nix-mode
 paredit

 markdown-mode 
 yaml-mode

 edit-indirect

 ####################

 helm
 helm-core
 helm-dash
 helm-make
 helm-swoop

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


];

########################################

unofficialPackages = [

];

in
############################################################

officialPackages ++ unofficialPackages

############################################################