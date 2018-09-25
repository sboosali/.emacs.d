########################################
########################################
{ nixpkgs ? import <nixpkgs> {}
}:

########################################
########################################
let 
########################################

emacsWithPackages =
 defaultEmacsPackages;

########################################

defaultEmacsPackages =
 nixpkgs.emacs25WithPackages;

########################################

# customEmacsPackages = ;

########################################

xwidgetsEmacsPackages =
    nixpkgs.emacsPackagesNg.overrideScope (super: self: {
        # use a custom version of emacs
        emacs = super.emacs.overrideAttrs (attributes: {
        
          withXwidgets = false;
          #^ for `xwidget-webkit-browse-url'.
          # enables the `--with-xwidgets` configuration flag.
          
          withGTK3 = true;
          withGTK2 = false;
          
          withCsrc = true;
          
#          configureFlags = (attributes.configureFlags or [])
#            ++ [ "--with-xwidgets" "--with-cairo" ];

          buildInputs = (attributes.buildInputs or [])
            ++ [ nixpkgs.webkitgtk ];  # nixpkgs.cairo

          inherit (nixpkgs) webkitgtk;

        });
        
        # ^
        # [Problem]
        #    configure: error: xwidgets requested but WebKitGTK+ not found.
        # [Solution]
        # 
        # 

        # use the unstable MELPA version of magit
        # magit = self.melpaPackages.magit;
    }).emacsWithPackages;

in 
########################################
########################################

emacsWithPackages

########################################
########################################









/*
let

inherit (nixpkgs) pkgs;

in
########################################
let

emacs26Packages =
  pkgs.emacsPackagesNgGen nixpkgs.emacs26;

myEmacs26Packages =
  emacs26Packages.overrideScope (myEmacsPackageOverrides nixpkgs.emacs26);

myEmacs26WithPackages =
  myEmacs26Packages.emacsWithPackages;

in
########################################
let

myPackages = [];

myEmacs = myEmacs26WithPackages myPackages;

in
########################################

*/









/*


nixpkgs.emacs26PackagesNg



----------------------------------------

https://github.com/NixOS/nixpkgs/issues/11503

myPkgs = pkgs: epkgs: with epkgs;
  [ ... a list of all the Emacs packages I use ... ];

myEmacsPackageOverrides = emacs: super: self: with self;
  let withPatches = pkg: patches:
    overrideDerivation pkg (attrs: { inherit patches; }); in

  super.melpaPackages // {

  inherit (pkgs) fetchurl fetchgit fetchFromGitHub;
  inherit (pkgs) stdenv;
  inherit (stdenv) mkDerivation lib;
  inherit (lib) overrideDerivation;

  ... many packages overridden here, including Org ...
};

emacs26PackagesNg = pkgs.emacsPackagesNgGen emacs26;

customEmacs26Packages =
  emacs26PackagesNg.overrideScope (myEmacsPackageOverrides emacs26);

emacs26Env = pkgs.myEnvFun {
  name = "emacs26";
  buildInputs = [ (customEmacs26Packages.emacsWithPackages myPkgs) ];
};

----------------------------------------



----------------------------------------

*/



/*
########################################
{ systemPackages ? import <nixpkgs> {}
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
*/
