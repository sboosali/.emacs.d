melpa: stable: elpa: org: 

########################################
with melpa; 

[


via `emacsWithPackages`:

{
myEmacs = emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
      use-package
      company
      ...
    ]));
}

 ####################
 # melpa...

 use-package
 dash
 s
 
 real-auto-save
 tabbar

 color-theme
 smooth-scrolling
 centered-cursor-mode

 flycheck
 multi-term

 haskell-mode
 markdown-mode 
 nix-mode
 paredit

 magit          # <C-x g>
 projectile

]
########################################
