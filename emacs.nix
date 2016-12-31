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
 magit          # ; Integrate git <C-x g>
 helm
 undo-tree      # ; <C-x u> to show the undo tree
 zoom-frm       # ; increase/decrease font size for all buffers %lt;C-x C-+>
 idris-mode
 # erc # builtin
 haskell-mode
 nix-mode
 projectile
 # intero
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
 deft
 paredit
 
  ]) ++ (with epkgs.melpaStablePackages; [
  
  ]) ++ (with epkgs.orgPackages; [
    org
    
  ]) ++ (with epkgs.elpaPackages; [
    auctex         # ; LaTeX mode

  ]))

