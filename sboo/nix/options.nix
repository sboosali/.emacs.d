/* 
 *
 * vendored from « <home-manager>/modules/programs/emacs.nix »
 */

#------------------------------------------------#
# Inputs ----------------------------------------#
#------------------------------------------------#

{ config, lib, pkgs, ... }:

with lib;

##################################################
let
#------------------------------------------------#

cfg = config.programs.emacs;

#------------------------------------------------#

# (Copied from all-packages.nix, with modifications to support overrides.)

emacsPackages =
    let
      epkgs = pkgs.emacsPackagesNgGen cfg.package;
    in
      epkgs.overrideScope' cfg.overrides;

#------------------------------------------------#

emacsWithPackages = emacsPackages.emacsWithPackages;

#------------------------------------------------#
in
##################################################
let
#------------------------------------------------#

options = {

  #----------------------------#

  enable        = mkEnableOption "Emacs";

  #----------------------------#

  package       = mkOption {
        type             = types.package;
        default          = pkgs.emacs;
        defaultText      = "pkgs.emacs";
        example          = literalExample "pkgs.emacs25-nox";
        description      = "The Emacs package to use.";
  };

  #----------------------------#

  extraPackages = mkOption {
        default          = self: [];
        defaultText      = "epkgs: []";
        example          = literalExample "epkgs: [ epkgs.emms epkgs.magit ]";
        description      = "Extra packages available to Emacs.";
  };

  #----------------------------#

  overrides     = mkOption {
        default          = self: super: {};
        defaultText      = "self: super: {}";
        example          = literalExample ''
          self: super: rec {
            haskell-mode = self.melpaPackages.haskell-mode;
            # ...
      };
        '';
        description      = ''
          Allows overriding packages within the Emacs package set.
        '';
  };

  #----------------------------#

  finalPackage  = mkOption {
        type             = types.package;
        visible          = false;
        readOnly         = true;
        description      = ''
          The Emacs package including any overrides and extra packages.
        '';
  };

  #----------------------------#

};

#------------------------------------------------#
in
##################################################
{

#------------------------------------------------#

  meta.maintainers = [ maintainers.sboosali ];

#------------------------------------------------#

  config = mkIf cfg.enable {

    home.packages               = [ cfg.finalPackage ];
    programs.emacs.finalPackage = emacsWithPackages cfg.extraPackages;

  };

#------------------------------------------------#

  options = {

    programs.emacs = options;

  };

#------------------------------------------------#

}
##################################################