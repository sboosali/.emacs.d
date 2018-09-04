############################################################
self: super:
############################################################
{

  ########################################
  /* e.g.
  magit = self.melpaPackages.magit;
  */

  ########################################
  word-count-mode = self.compileEmacsFiles {
    name = "word-count-mode";
    src = fetchFromGitHub {
      owner = "tomaszskutnik";
      repo = "word-count-mode";
      rev = "6267c98e0d9a3951e667da9bace5aaf5033f4906";
      sha256 = "1pvwy6dm6pwm0d8dd4l1d5rqk31w39h5n4wxqmq2ipwnxrlxp0nh";
      # date = 2015-07-16T22:37:17+02:00;
    };
  };

  ########################################
  use-package = self.melpaBuild {
   pname = "use-package";
   version = "20180127.1411";
   src = ~/src/dot-emacs/lisp/use-package;
   recipeFile = fetchurl {
     url = "https://raw.githubusercontent.com/milkypostman/melpa/51a19a251c879a566d4ae451d94fcb35e38a478b/recipes/use-package";
     sha256 = "0d0zpgxhj6crsdi9sfy30fn3is036apm1kz8fhjg1yzdapf1jdyp";
     name = "use-package";
   };
   packageRequires = [ self.bind-key self.emacs ];
   meta = {
     homepage = "https://melpa.org/#/use-package";
     license = lib.licenses.free;
   };
 };


  ########################################
  highlight = compileEmacsWikiFile {
    name = "highlight.el";
    sha256 = "0masypzcpqimqb8r0x5d53yc4r4k2g17qr8bg0ysp5li2pmdll9d";
    # date = 2018-02-21T17:21:42-0800;
  };

  ########################################

    ascii = compileEmacsWikiFile {
      name = "ascii.el";
      sha256 = "05fjsj5nmc05cmsi0qj914dqdwk8rll1d4dwhn0crw36p2ivql75";
      # date = 2018-02-21T17:21:27-0800;
    };

    backup-each-save = compileEmacsWikiFile {
      name = "backup-each-save.el";
      sha256 = "0b9vvi2m0fdv36wj8mvawl951gjmg3pypg08a8n6rzn3rwg0fwz7";
      # date = 2018-02-21T17:21:29-0800;
    };

    browse-kill-ring-plus = compileEmacsWikiFile {
      name = "browse-kill-ring+.el";
      sha256 = "1s32f70lc1gnllqqfw8zqr5n743rf0yfifqljsl210vnb5zg4zkj";
      # date = 2018-02-21T17:21:31-0800;

      buildInputs = [ self.browse-kill-ring ];
      patches = [ ./emacs/patches/browse-kill-ring-plus.patch ];
    };

    col-highlight = compileEmacsWikiFile {
      name = "col-highlight.el";
      sha256 = "0na8aimv5j66pzqi4hk2jw5kk00ki99zkxiykwcmjiy3h1r9311k";
      # date = 2018-02-21T17:21:33-0800;

      buildInputs = [ self.vline ];
    };

  ########################################

  ########################################

  ########################################

  ########################################

  ########################################
  pdf-tools = lib.overrideDerivation super.pdf-tools (attrs: {
      src = fetchFromGitHub {
        owner = "politza";
        repo = "pdf-tools";
        rev = "60d12ce15220d594e8eb95f4d072e2710cddefe0";
        sha256 = "1s8zphbd7k1ifdlisy894cg4mrkiq1rl2qk8x10njp1i596hz1fm";
        # date = 2018-04-29T18:31:04+02:00;
      };
    });

  ########################################
  org = mkDerivation rec {
      name = "emacs-org-${version}";
      version = "20160421";
      src = fetchFromGitHub {
        owner  = "jwiegley";
        repo   = "org-mode";
        rev    = "db5257389231bd49e92e2bc66713ac71b0435eec";
        sha256 = "073cmwgxga14r4ykbgp8w0gjp1wqajmlk6qv9qfnrafgpxic366m";
      };
      preBuild = ''
        rm -f contrib/lisp/org-jira.el
        makeFlagsArray=(
          prefix="$out/share"
          ORG_ADD_CONTRIB="org* ox*"
        );
      '';
      preInstall = ''
        perl -i -pe "s%/usr/share%$out%;" local.mk
      '';
      buildInputs = [ self.emacs ] ++ (with pkgs; [ texinfo perl which ]);
      meta = {
        homepage = "https://elpa.gnu.org/packages/org.html";
        license = lib.licenses.free;
      };
    };

  ########################################

}
########################################
