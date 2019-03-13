{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.lib.overrideDerivation (pkgs.emacs.override { }) (attrs: {
  });
  emacsPackages = (pkgs.emacsPackagesNgGen myEmacs).overrideScope (super: self: {
    # overrides for packages broken in unstable

    # common
    alert                = self.melpaPackages.alert;
    all-the-icons        = self.melpaPackages.all-the-icons;
    anzu                 = self.melpaPackages.anzu;
    anaconda-mode        = self.melpaPackages.anaconda-mode;
    async                = self.melpaPackages.async;
    auto-complete        = self.melpaPackages.auto-complete;
    avy                  = self.melpaPackages.avy;
    bind-key             = self.melpaPackages.bind-key;
    bind-map             = self.melpaPackages.bind-map;
    company              = self.melpaPackages.company;
    counsel              = self.melpaPackages.counsel;
    counsel-projectile   = self.melpaPackages.counsel-projectile;
    ctable               = self.melpaPackages.ctable;
    dash                 = self.melpaPackages.dash;
    dash-functional      = self.melpaPackages.dash-functional;
    deferred             = self.melpaPackages.deferred;
    diminish             = self.melpaPackages.diminish;
    eclim                = self.melpaPackages.eclim;
    epl                  = self.melpaPackages.epl;
    ess                  = self.melpaPackages.ess;
    # eval-sexp-fu         = self.melpaPackages.eval-sexp-fu;
    evil                 = self.melpaPackages.evil;
    evil-anzu            = self.melpaPackages.evil-anzu;
    evil-args            = self.melpaPackages.evil-args;
    f                    = self.melpaPackages.f;
    flx                  = self.melpaPackages.flx;
    flycheck             = self.melpaPackages.flycheck;
    flyspell-correct     = self.melpaPackages.flyspell-correct;
    fringe-helper        = self.melpaPackages.fringe-helper;
    ghc                  = self.melpaPackages.ghc;
    git                  = self.melpaPackages.git;
    git-commit           = self.melpaPackages.git-commit;
    git-gutter           = self.melpaPackages.git-gutter;
    git-gutter-plus      = self.melpaPackages.git-gutter-plus;
    gntp                 = self.melpaPackages.gntp;
    google-translate     = self.melpaPackages.google-translate;
    goto-chg             = self.melpaPackages.goto-chg;
    haskell-mode         = self.melpaPackages.haskell-mode;
    haml-mode            = self.melpaPackages.haml-mode;
    hcl-mode             = self.melpaPackages.hcl-mode;
    helm                 = self.melpaPackages.helm;
    helm-core            = self.melpaPackages.helm-core;
    helm-dash            = self.melpaPackages.helm-dash;
    highlight            = self.melpaPackages.highlight;
    ht                   = self.melpaPackages.ht;
    htmlize              = self.melpaPackages.htmlize;
    hydra                = self.melpaPackages.hydra;
    iedit                = self.melpaPackages.iedit;
    inf-ruby             = self.melpaPackages.inf-ruby;
    inflections          = self.melpaPackages.inflections;
    js2-mode             = self.melpaPackages.js2-mode;
    json-reformat        = self.melpaPackages.json-reformat;
    json-snatcher        = self.melpaPackages.json-snatcher;
    julia-mode           = self.melpaPackages.julia-mode;
    log4e                = self.melpaPackages.log4e;
    ledger-mode          = self.melpaPackages.ledger-mode;
    lsp-mode             = self.melpaPackages.lsp-mode;
    ivy                  = self.melpaPackages.ivy;
    magit                = self.melpaPackages.magit;
    magit-popup          = self.melpaPackages.magit-popup;
    markdown-mode        = self.melpaPackages.markdown-mode;
    memoize              = self.melpaPackages.memoize;
    multiple-cursors     = self.melpaPackages.multiple-cursors;
    mustache             = self.melpaPackages.mustache;
    nixos-options        = self.melpaPackages.nixos-options;
    org                  = self.orgPackages.org-plus-contrib; # we always want org-plus-contrib
    org-category-capture = self.melpaPackages.org-category-capture;
    org-projectile       = self.melpaPackages.org-projectile;
    parent-mode          = self.melpaPackages.parent-mode;
    packed               = self.melpaPackages.packed;
    pcre2el              = self.melpaPackages.pcre2el;
    php-mode             = self.melpaPackages.php-mode;
    pkg-info             = self.melpaPackages.pkg-info;
    popup                = self.melpaPackages.popup;
    popwin               = self.melpaPackages.popwin;
    pos-tip              = self.melpaPackages.pos-tip;
    projectile           = self.melpaPackages.projectile;
    projectile-rails     = self.melpaPackages.projectile-rails;
    pythonic             = self.melpaPackages.pythonic;
    rake                 = self.melpaPackages.rake;
    request              = self.melpaPackages.request;
    request-deferred     = self.melpaPackages.request-deferred;
    s                    = self.melpaPackages.s;
    sass-mode            = self.melpaPackages.sass-mode;
    simple-httpd         = self.melpaPackages.simple-httpd;
    skewer-mode          = self.melpaPackages.skewer-mode;
    smartparens          = self.melpaPackages.smartparens;
    swiper               = self.melpaPackages.swiper;
    tablist              = self.melpaPackages.tablist;
    term-run             = self.melpaPackages.term-run;
    tern                 = self.melpaPackages.tern;
    typescript-mode      = self.melpaPackages.typescript-mode;
    # undo-tree            = self.melpaPackages.undo-tree;
    yasnippet            = self.melpaPackages.yasnippet;
    web-completion-data  = self.melpaPackages.web-completion-data;
    with-editor          = self.melpaPackages.with-editor;
    xterm-color          = self.melpaPackages.xterm-color;
  });
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;

in {

  myEmacsWithPackages = emacsPackages.emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
  ]) ++ (with epkgs.melpaPackages; [
    # common
    ace-link
    ace-window
    ac-ispell
    auto-complete
    auto-dictionary
    auto-highlight-symbol
    alert
    async
    # highlight cursor when scrolling
    beacon # seq is in elpa
    beginend
    clean-aindent-mode
    column-enforce-mode
    company-quickhelp
    company-statistics
    # connection # broken as of 2017-09-19
    counsel
    counsel-dash
    counsel-gtags
    counsel-projectile
    define-word
    # dictionary # broken as of 2017-09-11
    diff-hl
    direnv
    dumb-jump
    # eval-sexp-fu
    evil
    evil-anzu
    evil-args
    evil-avy
    evil-commentary
    evil-ediff
    evil-escape
    evil-exchange
    evil-goggles
    evil-iedit-state
    evil-indent-plus
    evil-lisp-state
    evil-matchit
    evil-mc
    evil-numbers
    # evil-search-highlight-persist
    evil-smartparens
    evil-surround
    evil-tutor
    evil-visual-mark-mode
    evil-visualstar
    expand-region
    eyebrowse
    fancy-battery
    fill-column-indicator
    flx
    flx-ido
    flyspell-correct-ivy
    # flyspell-popup # this has been disabled
    fuzzy
    golden-ratio
    # help-fns
    # help-fns-plus
    # highlight
    # highlight-indentation
    # highlight-numbers
    # highlight-parentheses
    hl-todo
    htmlize
    hungry-delete
    indent-guide
    # info-plus
    # insert-shebang broken as of 8/9
    ivy-hydra
    # link # broken as of 2017-09-19
    # link-hint       # use avy to open or copy visible urls - broken as of 3/9
    lorem-ipsum
    macrostep
    move-text
    multi-term
    multiple-cursors
    nameless          # hide current package name everywhere in elisp code
    neotree
    nlinum-relative   # nlinum is in elpa
    openwith
    open-junk-file
    paradox
    pdf-tools
    persp-mode
    popwin
    powerline
    projectile
    rainbow-delimiters
    restart-emacs
    shell-pop
    smartparens
    smeargle
    smex
    spaceline
    swiper
    tablist # adds marks and filters to tabulated-list-mode
    # undo-tree
    use-package
    uuidgen
    vi-tilde-fringe
    visual-fill-column
    volatile-highlights
    wgrep
    which-key
    winum

    # application modes
    evil-mu4e
    mu4e-alert
    mu4e-maildirs-extension
    spray
    wttrin

    # git
    evil-magit
    git
    gitattributes-mode
    git-command
    git-commit
    gitconfig-mode
    git-gutter-plus
    git-gutter-fringe
    git-gutter-fringe-plus
    gitignore-mode
    git-link
    git-messenger
    # git-timemachine
    magit
    magit-gitflow
    magit-popup
    orgit

    # helm
    helm-make

    # eshell
    eshell-prompt-extras
    eshell-z
    esh-help
    fish-mode

    # general programming
    auto-yasnippet
    auto-compile
    aggressive-indent
    editorconfig
    flycheck
    flycheck-pos-tip
    ggtags
    # hide-comnt
    kurecolor
    lsp-mode # common server protocol for auto-completion
    vagrant
    vagrant-tramp
    zeal-at-point

    # languages
    coffee-mode
    emmet-mode
    hy-mode      # a lisp thing. Why do we need this?
    livid-mode   # live JS eval for skewer-mode
    # mmm-mode
    # powershell # broken as of 3/9
    skewer-mode
    terraform-mode

    # languages - C and related languages
    clang-format
    # cmake-mode # broken as of 3/9
    cmm-mode
    company-c-headers
    disaster

    # languages - crystal
    # crystal-mode # not in melpa yet
    ob-crystal

    # languages - elisp
    elisp-slime-nav

    # languages - elm
    elm-mode
    flycheck-elm

    # languages - ess - is this needed?
    ess
    ess-R-data-view
    # ess-R-object-popup # temporarily missing from melpa, so excluded in spacemacs as well
    ess-smart-equals

    # languages - haskell
    company-cabal
    company-ghc
    company-ghci
    flycheck-haskell
    ghc
    haskell-snippets
    hindent
    hlint-refactor
    intero
    # lsp-haskell # temporarily disabled. Check spacemacs/private/peter/packages.el for details

    # languages - java
    company-emacs-eclim

    # languages - javascript
    company-tern
    js-doc
    js2-refactor

    # languages - nix
    company-nixos-options
    nix-buffer
    nix-mode
    nix-sandbox
    pretty-sha-path

    # languages - php
    # drupal-mode # pulls in php-mode
    # php-auto-yasnippets # broken as of 2017-09-03
    # php-extras # not available
    php-mode
    phpcbf
    phpunit

    # languages - python
    anaconda-mode
    company-anaconda
    cython-mode
    live-py-mode
    pip-requirements
    pyenv-mode
    pytest
    pythonic
    pyvenv
    py-isort
    yapfify

    # languages - ruby
    bundler
    chruby
    enh-ruby-mode
    evil-rails
    feature-mode
    minitest
    projectile-rails
    rbenv
    robe
    rspec-mode
    rubocop
    ruby-test-mode
    ruby-tools
    rvm
    ws-butler  # clean up white spaces

    # languages - salesforce
    salesforce-utils

    # languages - shell
    company-shell

    # languages - sql
    sqlup-mode
    sql-indent

    # languages - typescript
    tide

    # languages - web
    company-web
    pug-mode
    tagedit
    web-beautify
    web-mode

    # markup
    ansible
    ansible-doc
    company-ansible
    gh-md
    jinja2-mode
    json-mode
    less-css-mode
    markdown-toc
    mustache
    sass-mode
    scss-mode
    slim-mode
    toml-mode
    yaml-mode

    # text -> graphics modes
    gnuplot
    graphviz-dot-mode
    pandoc-mode

    # configuration files
    dactyl-mode
    # muttrc-mode
    vimrc-mode

    # org-mode
    ob-translate
    org-autolist
    org-download
    org-jira
    org-page
    org-pomodoro
    org-present
    # org-projectile
    org-table-sticky-header
    org-trello
    ox-pandoc
    ox-reveal
    secretaria
    timesheet
    toc-org

    # main modes
    flycheck-ledger
    # hledger-mode # we use ledger-mode for hledger as well
    ledger-mode

    # themes
    apropospriate-theme
    # material-theme
    solarized-theme
    zenburn-theme
    # zerodark-theme

  ]) ++ (with epkgs.elpaPackages; [
    adaptive-wrap
    auctex         # LaTeX mode
    csv-mode
    let-alist
    nlinum
    spinner

  ]) ++ (with epkgs.orgPackages; [
    org-plus-contrib

  ]) ++ (with pkgs; [
    aspell
    mu
    openssh_with_kerberos
  ]));
}
