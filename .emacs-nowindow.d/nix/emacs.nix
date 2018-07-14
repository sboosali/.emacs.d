make Emacs load a custom config file. Create a nix-package that provides a

  default.el 

file in 

  /share/emacs/site-start/

Emacs will load this file automatically when it starts.

{
   myEmacsConfig = writeText "default.el";

   myEmacs = emacsWithPackages (... [

      (runCommand "default.el" {} ''
mkdir -p "$out/share/emacs/site-lisp"
cp ${myEmacsConfig} "$out/share/emacs/site-lisp/default.el"
'')
      ...
      use-package
      company
    ]));

}

########################################

/*

Compatibility:

- this (i.e. $out/share/emacs/site-lisp/default.el) loads in addition to the user's presonal config (i.e. init.el), and before it.

- can always be disabled it by passing the `-q` command-line argument of the `emacs` executable.

*/

########################################