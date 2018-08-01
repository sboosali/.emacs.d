melpa: stable: elpa: org: 

########################################
let

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

];

unofficialPackages = [
];

in
########################################

officialPackages ++ unofficialPackages

########################################