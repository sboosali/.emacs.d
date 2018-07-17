########################################
{ systemPackages
}:
########################################
let

myPackages = 
 (import ./overrides/packages.nix);

myAttributes =
 (import ./overrides/attributes.nix);

in
########################################
let

emacsScope = super: self: {

  emacs =
    super.emacs.overrideAttrs
     (attributes:
       myAttributes { inherit attributes; })

  # ^ [1]
  # use a custom emacs.
  # e.g. newer version, extra flags, etc.
  # note:
  # the dynamic modules flag is already given in
  # (the contemporary) nixpkgs.

  ###magit = self.melpaPackages.magit;

  # ^ [2]
  # use a custom package.
  # e.g. the unstable MELPA version of magit:

 } // (myPackages self super);

myEmacs =
 systemPackages. (emacsScope);

in
########################################
myEmacs
########################################