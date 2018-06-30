{ nixpkgs           ? import <nixpkgs> {}
, emacsWithPackages ? nixpkgs.emacs26WithPackages
}:

########################################
let

utilities = (import ./utilities.nix) emacsWithPackages;

packages = import ./packages.nix;

in
########################################
let

myEmacs = utilities.emacsWith packages;

in
########################################

myEmacs

########################################

/* NOTES

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

/* NOTES

non-repository emacs-package (e.g. `intero` from GitHub):

# intero = callPackage ({ company, emacs, fetchFromGitHub, fetchurl, flycheck, haskell-mode, lib, melpaBuild }:
#     melpaBuild {
#         pname = "intero";
#         version = "20161218.917";
#         src = fetchFromGitHub {
#           owner = "commercialhaskell";
#           repo = "intero";
#           rev = "59ad0002b16a77c2de0cc18862e8e1842c00d6c4";
#           sha256 = "1678zjrdawks59l95x6qsp01man8k7r3si6qk1bb6dz261dh9adl";
#         };
#         recipeFile = fetchurl {
#           url = "https://raw.githubusercontent.com/milkypostman/melpa/1b56ca344ad944e03b669a9974e9b734b5b445bb/recipes/intero";
#           sha256 = "15n7ipsq8ylmq4blsycpszkx034j9sb92vqvaz30j5v307fmvs99";
#           name = "intero";
#         };
#         packageRequires = [ company emacs flycheck haskell-mode ];
#         meta = {
#           homepage = "https://melpa.org/#/intero";
#           license = lib.licenses.free;
#         };
#       }) {};

*/

########################################

/* NOTES

- custom emacs executable, e.g.:

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

- nixpkgs/pkgs/build-support/emacs/wrapper.nix:

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

########################################
