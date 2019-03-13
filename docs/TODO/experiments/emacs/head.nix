######################################## TODO work-in-progress
self: 
pkgs:
########################################
let

emacsHEAD =
  with pkgs;

  stdenv.lib.overrideDerivation
   (pkgs.emacs26.override { srcRepo = true; }) (attrs:
     rec {
     
       name = "emacs-${version}${versionModifier}";
       version = "27.0";
       versionModifier = ".50";
     
       appName = "ERC";
       bundleName = "nextstep/ERC.app";
       iconFile = ./emacs/Chat.icns;
     
       buildInputs = emacs26.buildInputs ++
         [ git libpng.dev libjpeg.dev libungif libtiff.dev librsvg.dev
           imagemagick.dev ];
     
       patches = lib.optionals stdenv.isDarwin
         [ ./emacs/patches/at-fdcwd.patch
           ./emacs/patches/emacs-26.patch ];
     
       CFLAGS = "-O0 -g3 -DMAC_OS_X_VERSION_MAX_ALLOWED=101200";
     
       configureFlags = [ "--with-modules" ] ++
        [ "--with-ns" "--disable-ns-self-contained"
          "--enable-checking=yes,glyphs"
          "--enable-check-lisp-object-type" ];
     
       src = ~/src/emacs;
     
       postPatch = ''
         sed -i 's|/usr/share/locale|${gettext}/share/locale|g' \
           lisp/international/mule-cmds.el
         sed -i 's|nextstep/Emacs\.app|${bundleName}|' configure.ac
         sed -i 's|>Emacs<|>${appName}<|' nextstep/templates/Info.plist.in
         sed -i 's|Emacs\.app|${appName}.app|' nextstep/templates/Info.plist.in
         sed -i 's|org\.gnu\.Emacs|org.gnu.${appName}|' nextstep/templates/Info.plist.in
         sed -i 's|Emacs @version@|${appName} @version@|' nextstep/templates/Info.plist.in
         sed -i 's|EmacsApp|${appName}App|' nextstep/templates/Info.plist.in
         if [ -n "${iconFile}" ]; then
           sed -i 's|Emacs\.icns|${appName}.icns|' nextstep/templates/Info.plist.in
         fi
         sed -i 's|Name=Emacs|Name=${appName}|' nextstep/templates/Emacs.desktop.in
         sed -i 's|Emacs\.app|${appName}.app|' nextstep/templates/Emacs.desktop.in
         sed -i 's|"Emacs|"${appName}|' nextstep/templates/InfoPlist.strings.in
         rm -fr .git
         sh autogen.sh
       '';
     
       postInstall = ''
         mkdir -p $out/share/emacs/site-lisp
         cp ${./emacs/site-start.el} $out/share/emacs/site-lisp/site-start.el
         $out/bin/emacs --batch -f batch-byte-compile $out/share/emacs/site-lisp/site-start.el
         rm -rf $out/var
         rm -rf $out/share/emacs/${version}/site-lisp
         for srcdir in src lisp lwlib ; do
           dstdir=$out/share/emacs/${version}/$srcdir
           mkdir -p $dstdir
           find $srcdir -name "*.[chm]" -exec cp {} $dstdir \;
           cp $srcdir/TAGS $dstdir
           echo '((nil . ((tags-file-name . "TAGS"))))' > $dstdir/.dir-locals.el
         done
         mkdir -p $out/Applications
         if [ "${appName}" != "Emacs" ]; then
             mv ${bundleName}/Contents/MacOS/Emacs ${bundleName}/Contents/MacOS/${appName}
         fi
         if [ -n "${iconFile}" ]; then
           cp "${iconFile}" ${bundleName}/Contents/Resources/${appName}.icns
         fi
         mv ${bundleName} $out/Applications
       '';
     });
     
convertForERC = epkgs: pkgs.stdenv.lib.overrideDerivation epkgs (attrs: {
  installPhase = attrs.installPhase + ''
    if [ -d "$emacs/Applications/ERC.app" ]; then
      mkdir -p $out/Applications/ERC.app/Contents/MacOS
      cp -r $emacs/Applications/ERC.app/Contents/Info.plist \
            $emacs/Applications/ERC.app/Contents/PkgInfo \
            $emacs/Applications/ERC.app/Contents/Resources \
            $out/Applications/ERC.app/Contents
      makeWrapper $emacs/Applications/ERC.app/Contents/MacOS/ERC \
                  $out/Applications/ERC.app/Contents/MacOS/ERC \
                  --suffix EMACSLOADPATH ":" "$deps/share/emacs/site-lisp:"
    fi
  '';
});

in
########################################