/* 


Querying:

nix-env -f "<nixpkgs>" -qaP -A emacsPackagesNg.melpaPackages | grep

Building: 

nix-build emacs.nix

Running:

./result/bin/emacs

Both:

nix-build emacs.nix && ./result/bin/emacs


https://nixos.org/wiki/Emacs_configuration

*/
{ pkgs ? import <nixpkgs> {} }: 


let
  myEmacs = pkgs.emacs25; 
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages; 
in

  emacsWithPackages (epkgs: ([
  
  ]) ++ (with epkgs.melpaPackages; [

 #intero
 dante
# dhall-mode
 magit          # ; Integrate git <C-x g>
# helm
 undo-tree      # ; <C-x u> to show the undo tree
# zoom-frm       # ; increase/decrease font size for all buffers %lt;C-x C-+>
# idris-mode
 # erc # builtin
 haskell-mode
 nix-mode
 projectile
 multi-term
 magit
 # ido-complete-space-or-hyphen
 tabbar
 smooth-scrolling
 centered-cursor-mode
 exec-path-from-shell
 dash
 s
 projectile
 flx-ido
 evil
 #help-fns-plus  # aka help-fns+ 
 window-purpose
 #replace-plus
# deft
 paredit
 color-theme
use-package
flycheck
 
  ]) ++ (with epkgs.melpaStablePackages; [
  
  ]) ++ (with epkgs.orgPackages; [
    org
    
  ]) ++ (with epkgs.elpaPackages; [
    auctex         # ; LaTeX mode
    # vlfi

  ]))

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

