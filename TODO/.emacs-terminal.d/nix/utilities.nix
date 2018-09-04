emacsWithPackages:

########################################
let

in
########################################
{

 /*
  *
  * withRepositories :: [PackageSet] -> [PackageSet] -> [PackageSet] -> [PackageSet] -> [PackageSet]
  *
  * takes callback/continuation for scoping.
  *
  */
 emacsWith = withRepositories:
  emacsWithPackages (epkgs:
    withRepositories
        epkgs.melpaPackages
        epkgs.melpaStablePackages
        epkgs.elpaPackages
        epkgs.orgPackages);

}
########################################
/*

this file is a "module", returning an attr-set of utility functions.

*/
########################################
