{ nixpkgs ? import <nixpkgs> {}
, emacs         ? nixpkgs.emacs
, emacsPackages ? nixpkgs.emacs26Packages
}:

########################################
let

originalEmacsPackages = emacsPackages emacs;

customEmacsPackages = scopedEmacsPackages;

scopedEmacsPackages = originalEmacsPackages.overrideScope emacsScope; 

emacsScope = super: self: {

  # # [1] use a custom emacs.
  # # e.g. newer version, extra flags, etc.
  # emacs = ...;

  #^ NOTE 
  # the dynamic modules flag is already given in the contemporary nixpkgs:
  # emacs = super.emacs.overrideAttrs (attributes: { 
  #   attributes.configureFlags ++ [ "--with-modules" ] ; 
  # }); 

  # # [2] use a custom package.
  # # e.g. the unstable MELPA version of magit:
  # magit = self.melpaPackages.magit;

};

# customEmacsPackages.emacsWithPackages (epkgs: [ epkgs.evil epkgs.magit ])

# emacsPackages = nixpkgs.emacsPackagesNgGen emacs;

/* 
 *
 * withRepositories :: [PackageSet] -> [PackageSet] -> [PackageSet] -> [PackageSet] -> [PackageSet] 
 *
 * takes callback/continuation for scoping
 *
 */
emacsWith = withRepositories: 
  customEmacsPackages.emacsWithPackages (epkgs: 
    withRepositories 
        epkgs.melpaPackages
        epkgs.melpaStablePackages
        epkgs.elpaPackages
        epkgs.orgPackages);

in
########################################
let

myEmacs = emacsWith (melpa: stable: elpa: org: with melpa; [

 # melpa...

 avy

 dante
 haskell-mode 

 markdown-mode
 
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

 ]);

in
########################################

myEmacs

########################################

/* NOTES...

Arguments:

    The emacs version/configuration is `emacs26`, i.e. "Emacs 26.1", by default.

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

/* nixpkgs/pkgs/build-support/emacs/wrapper.nix:

# Usage

`emacsWithPackages` takes a single argument: a function from a package
set to a list of packages (the packages that will be available in
Emacs). For example,
```
emacsWithPackages (epkgs: [ epkgs.evil epkgs.magit ])
```
All the packages in the list should come from the provided package
set. It is possible to add any package to the list, but the provided
set is guaranteed to have consistent dependencies and be built with
the correct version of Emacs.

# Overriding

`emacsWithPackages` inherits the package set which contains it, so the
correct way to override the provided package set is to override the
set which contains `emacsWithPackages`. For example, to override
`emacsPackagesNg.emacsWithPackages`,
```
let customEmacsPackages =
      emacsPackagesNg.overrideScope (super: self: {
        # use a custom version of emacs
        emacs = ...;
        # use the unstable MELPA version of magit
        magit = self.melpaPackages.magit;
      });
in customEmacsPackages.emacsWithPackages (epkgs: [ epkgs.evil epkgs.magit ])
```

*/


########################################
