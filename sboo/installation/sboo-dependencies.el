;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-emacs-packages-required

  '(bind-key
    use-package

    ;;

    dash           ; (the `-` prefix)
    s              ; `s`trings
    f              ; `f`iles
    
    ;;

    haskell-mode
    dante           ; needs `ghc` (or `cabal`, or `stack`, etc) system-package
    intero          ; needs `stack` system-package

    ;;

    lsp-mode
    lsp-ui
    lsp-haskell

    ;;

    flycheck
    flycheck-haskell

    ;;

    exec-path-from-shell
    real-auto-save

    ;;

    tabbar
    shackle
    window-purpose

    ;;

    color-theme
    smooth-scrolling
    centered-cursor-mode

    ;;

    yasnippet

    ;;

    nix-mode
    paredit

    ;;

                                        ;magit          ; <C-x g>
    projectile
    direnv         ; needs `direnv` system-package
                                        ;multi-term
    neotree
    wgrep

    markdown-mode 
    edit-indirect

    json-mode
    yaml-mode

    ;;

    ;;

    helm
    helm-core
    helm-dash
    helm-make
    helm-swoop

    ;;

    modalka
                                        ; ^ https://github.com/mrkkrp/modalka/blob/master/README.md

    ;;god-mode
    ;;evil

    ;;

    anzu   
                                        ; ^ shows total search hits in mode line.
                                        ; c.f. `query-replace`.

    bm 
                                        ; ^ visual bookmarks

                                        ; csv-nav
                                        ;  ; ^ editing csv files

    deft
                                        ; ^ quick note taking and management

    expand-region

    page-break-lines
                                        ; ^ Convert the ^L (form feed) chars to horizontal lines

    wrap-region
                                        ; ^ wrap selection with punctuations, tags (org-mode, markdown-mode, ..)

    yaml-mode

                                        ;any-ini-mode ; EmacsWiki only

    ;;

    epc
    elnode
                                        ;emacs-web-server
    
    ;;
                                        ; for haskell-ide-engine:

    lsp-mode
    lsp-ui 
    lsp-haskell

    ;;
                                        ; from the "dired-hacks" megarepo:

    dired-filter
    dired-open
    dired-rainbow
    dired-subtree
    dired-ranger
                                        ;dired-list
    dired-collapse

    ;;

    move-text
    treemacs
    tabbar-ruler

    ;;

    request
                                        ; ^ https://tkf.github.io/emacs-request/manual.html

    ;;

    wgrep
                                        ; ^ https://github.com/mhayashi1120/Emacs-wgrep

    ;;

    ;;TODO repo;; org.org

    ;;

    palimpsest

    ;;

    which-key
                                        ; ^ https://github.com/justbur/emacs-which-key/blob/master/README.org

    ;;

    )

  "Install these Emacs packages; which are required for (and configured by) my Emacs configuration.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-configure-emacs-package-repositories! ()

  "Add MELPA to `package-archives'. 
  "

  (when (>= emacs-major-version 24)
    
    (require 'package)

    (progn
      (add-to-list 'package-archives
                   '("melpa-stable" . "http://stable.melpa.org/packages/")
                   t)
      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.milkbox.net/packages/")
                   t)
      ;; ^
      ;; « (`add-to-list' LIST-VAR ELEMENT &optional APPEND COMPARE-FN) »
      ())

    (package-initialize)

    ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-install-emacs-packages! (&optional RefreshAndForceReinstall)
  " Install everything in `sboo-emacs-packages-required'.

  With a Universal Argument (e.g. `C-u'), reconfigure the package manager
  (via `package-initialize' and `package-refresh-contents'),
  before installing the packages (via `package-install').

  "
  (interactive "P")

  ;;;(message "« RefreshAndForceReinstall = %s »" RefreshAndForceReinstall)

  (progn

    ;;TODO set coding system globally for url.el &al?
    (prefer-coding-system 'utf-8)
    
    (when RefreshAndForceReinstall
      (package-refresh-contents))

    (dolist (p sboo-emacs-packages-required)

      (when (or (not (package-installed-p p))
                RefreshAndForceReinstall)

        (package-install p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `melpa'
;;
;; MELPA is a package-repository.
;;
;; MELPA is updated daily and has the most packages (circa 2018).
;;
;; its repository is hosted at `https://melpa.org/'.
;;
;;

;; See
;;     - http://ergoemacs.org/emacs/emacs_package_system.html
;;     - 
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-dependencies)