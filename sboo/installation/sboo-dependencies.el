;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-emacs-packages-needed

  '(

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Meta-Configuration:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    use-package-el-get  ; `use-package` dependency
    bind-key            ; `use-package` dependency
    use-package

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Text/Buffer/Window Stuff:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    real-auto-save
    yasnippet

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Helm:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    async      ; `helm` dependency
    popup      ; `helm` dependency
    helm-core  ; `helm` dependency
    helm

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Development:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    projectile
    flycheck
    magit            ; git <C-x g>

    haskell-mode     ; haskell
    dante            ; haskell
    flycheck-haskell ; haskell

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Utilities:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    dash           ; (the `-` prefix)
    s              ; (`s`trings)
    f              ; (`f`iles)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   )

  "Packages I really need to be installed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-emacs-packages-wanted

  '(

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Window/Buffer Management:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    tabbar
    shackle
    window-purpose

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Development:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    paredit
    nix-mode

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Formats:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    markdown-mode
    json-mode
    yaml-mode
    csv-nav
    ;; ^ editing csv files

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Filesystem/Project Navigation/Searching:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    multi-term
    ;neotree
    bm
    ;; ^ visual bookmarks

    ;; from the "dired-hacks" megarepo:

    dired-filter
    dired-open
    dired-rainbow
    dired-subtree
    dired-ranger
   ;dired-list
    dired-collapse

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Text Manipulation/Visualization:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    expand-region

    page-break-lines
                                        ; ^ Convert the ^L (form feed) chars to horizontal lines

    wrap-region
                                        ; ^ wrap selection with punctuations, tags (org-mode, markdown-mode, ..)

    wgrep
                                        ; ^ https://github.com/mhayashi1120/Emacs-wgrep

    edit-indirect

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; (miscellaneous):
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    anzu
                                        ; ^ shows total search hits in mode line.
                                        ; c.f. `query-replace`.
    request
                                        ; ^ https://tkf.github.io/emacs-request/manual.html
    which-key
                                        ; ^ https://github.com/justbur/emacs-which-key/blob/master/README.org


    )

  "Packages I want (but don't need) to be installed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-emacs-packages

  sboo-emacs-packages-needed

  ;;;(append sboo-emacs-packages-needed
  ;;;        sboo-emacs-packages-wanted)

  "Install these Emacs packages. Required for (and configured by) my Emacs configuration.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-configure-emacs-package-repositories ()

  "Add MELPA to `package-archives'.
  "

  (when (>= emacs-major-version 24)

    (require 'package)

    (progn

      (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

      ;;;(add-to-list 'package-archives '("melpa"        . "http://melpa.milkbox.net/packages/") t)

      ;; ^
      ;; « (`add-to-list' LIST-VAR ELEMENT &optional APPEND COMPARE-FN) »
      ())

    (package-initialize)

    ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-install-emacs-packages! (&optional RefreshAndForceReinstall)
  "Install everything in `sboo-emacs-packages'.

  With a Universal Argument (e.g. `C-u'), reconfigure the package manager
  (via `package-initialize' and `package-refresh-contents'),
  before installing the packages (via `package-install').

  "
  (interactive "P")

  ;;;(message "« RefreshAndForceReinstall = %s »" RefreshAndForceReinstall)

  (progn

    (prefer-coding-system 'utf-8)      ;;TODO set coding system globally for url.el &al?

    (sboo-configure-emacs-package-repositories)

    (when RefreshAndForceReinstall
      (package-refresh-contents))

    (dolist (p sboo-emacs-packages)

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