# The Emacs package comes with some extra helpers to make it easier to configure. emacsWithPackages allows you to manage packages from ELPA. This means that you will not have to install that packages from within Emacs. For instance, if you wanted to use company, counsel, flycheck, ivy, magit, projectile, and use-package you could use this as a ~/.config/nixpkgs/config.nix override:

{ pkgs ? import <nixpkgs> {} }: 

let
  myEmacs = pkgs.emacs25; 
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages; 
in

  emacsWithPackages (epkgs: ([
  
  ]) ++ (with epkgs.melpaPackages; [
      nix-mode
      company
      counsel
      flycheck
      ivy
      magit
      projectile
      use-package
    ]))

# You can install it like any other packages via nix-env -iA myEmacs. However, this will only install those packages. It will not configure them for us. 

