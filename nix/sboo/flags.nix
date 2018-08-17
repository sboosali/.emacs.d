##################################################
{ version  ? "26" #####TODO

, xwidgets ? true
, gtk      ? 3

}:

##################################################
self: super:

##################################################
{

 emacs = super.emacs.overrideAttrs (attributes: {
        
          withGTK2 = gtk == 2;
          withGTK3 = gtk == 3;
          # ^ `lucid' if neither GTK.

          withXwidgets = xwidgets;
          #^ for `xwidget-webkit-browse-url'.
          # enables the `--with-xwidgets` configuration flag.
          
          # configureFlags = (attributes.configureFlags or []) ++ [ "" ];
        })

}
##################################################
#
##################################################