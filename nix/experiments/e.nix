{ nixpkgs            ? import <nixpkgs> {}
, pkgs               ? nixpkgs.pkgs
, emacs0             ? pkgs.emacs26
, emacsPackagesNgGen ? pkgs.emacsPackagesNgGen
, enableDebugInfo    ? (x: x)
}:
########################################

########################################

let

builtinPackages = epkgs:
 (with epkgs; [


  haskell-mode nix-mode idris-mode 

  yaml-mode json-mode markdown-mode 

  multiple-cursors
   
  magit magithub gitignore-mode 

  skewer-mode skewer-less skewer-reload-stylesheets 

  # htmlize ido-completing-read-plus json-reformat jvm-mode nixos-options paredit macrostep levenshtein rainbow-delimiters string-edit keyfreq uuidgen coffee-mode dockerfile-mode cmake-mode
  ]);

  emacsPackages = emacsPackagesNgGen (
    enableDebugInfo (emacs0.override {
      inherit (pkgs) alsaLib imagemagick acl gpm;
      inherit (pkgs.gnome3) gconf;
      withGTK3 = true; withGTK2 = false;
    }));

  emacsWithPackages = pfn:
    emacsPackages.emacsWithPackages (epkgs:
      (builtinPackages epkgs) ++ (pfn epkgs));

  emacs = emacsWithPackages (_: []);

in 
########################################
{

  inherit emacsPackages emacs emacsWithPackages;

}
########################################

/*NOTES

emacs26PackagesNg

*/

########################################
