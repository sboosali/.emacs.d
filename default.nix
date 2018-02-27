{ nixpkgs ? import <nixpkgs> {}
, emacs ? nixpkgs.emacs25
}: 

/* NOTES...

Arguments:

    The emacs version/configuration is `emacs25` by default.

Querying:

    nix-env -f "<nixpkgs>" -qaP -A emacsPackagesNg.melpaPackages | grep ...
    nix-env -f "<nixpkgs>" -qaP -A emacsPackagesNg.orgPackages | grep ...

Building: 

    nix-build default.nix

Running:

    ./result/bin/emacs

Build and Launch:

    nix-build default.nix && ./result/bin/emacs

    # aka

    ../emacs.sh

See:

    https://nixos.org/wiki/Emacs_configuration
*/

########################################
let

emacsPackages = nixpkgs.emacsPackagesNgGen emacs;

/* 
 *
 * withRepositories :: [PackageSet] -> [PackageSet] -> [PackageSet] -> [PackageSet] -> [PackageSet] 
 *
 * takes callback/continuation for scoping
 *
 */
emacsWith = withRepositories: 
  emacsPackages.emacsWithPackages (epkgs: 
    withRepositories 
        epkgs.melpaPackages
        epkgs.melpaStablePackages
        epkgs.elpaPackages
        epkgs.orgPackages);

in
########################################

emacsWith (melpa: stable: elpa: org: with melpa; [

 # melpa...

 dante
 haskell-mode 

 nix-mode
 paredit

 magit          # <C-x g>
 flycheck
 projectile
 multi-term

 helm
 real-auto-save

 tabbar
 smooth-scrolling
 centered-cursor-mode

 use-package
 dash
 s

 # org...

 org.org

 # elpa...

 elpa.auctex

 ])

########################################
