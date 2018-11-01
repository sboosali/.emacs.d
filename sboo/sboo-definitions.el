;;; Core Definitions (no Statements) for SBoo's Emacs Configuration -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun truename-as-directory (FilePath)
  "Return « `FilePath'/ ».

Return the true name of `FilePath', as a directory path:

* an absolute path, 
* with symbolic links resolved (but not hard links),
* with « ~/... » and « $HOME/... » expanded.

Calls `file-name-as-directory' and `file-truename'."

  (file-name-as-directory (file-truename FilePath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment Variables ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-environment-variable-install "EMACS_INSTALL"

  "See `sboo-install-p'.

Example Usage: « $ EMACS_INSTALL=t emacs ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-install-p ()

  "Whether to install packages. (TODO and how to install them).

(e.g. when Emacs is first launched on a new computer)."

  (pcase (getenv sboo-environment-variable-install)

      ('()   nil)
      ("nil" nil)
      ("t"     t)
      
      ("nixpkgs"    'nixpkgs)
      ("submodules" 'submodules)
      ("melpa"      'melpa)

      (_ nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-install-submodules-p ()
  "Whether to load the `sboo-critical-packages' from vendored submodules.

Wraps `sboo-install-p'."

  (pcase (sboo-install-p)

      ('submodules   t)
      (_           nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst emacs-directory

  (truename-as-directory (or user-emacs-directory
			     "~/.emacs.d/"))

  "The root directory of the user's emacs configuration.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emacs-file (FilePath)
  "Return « `emacs-directory'/`FilePath' ».

  i.e. Return the relative filepath `FilePath', 
  as an absolute filepath, under `emacs-directory'.

  Calls `file-truename'.
  "

  (file-truename (concat emacs-directory
			 FilePath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emacs-subdir (FilePath)
  "Return « `emacs-directory'/`FilePath'/ ».

  i.e. Return the relative directory `FilePath', 
  as an absolute sub-directory of `emacs-directory'.

  Calls `file-name-as-directory' and `file-truename'.
  "

  (truename-as-directory (concat emacs-directory
				 FilePath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-lisp-directory

  (emacs-subdir "lisp/")

  "Directory with vendored (individual) ELisp files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-installed-package-directory

  (or (bound-and-true-p package-user-dir)
      (emacs-subdir "elpa/"))

  "Directory where `package.el' should install ELisp packages.
`package-user-dir' by default.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-vendored-package-directory

  (emacs-subdir "submodules/")

  "Directory which contains any vendored ELisp packages (as subdirectories).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-root-directory

  (emacs-subdir "sboo/")

  "The root directory of my personal configuration.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-subdir (FilePath)
  "Return « `sboo-root-directory'/`FilePath'/ ».

  i.e. Return the relative directory `FilePath', 
  as an absolute sub-directory of `sboo-root-directory'.

  Calls `file-name-as-directory' and `file-truename'.
  "

  (truename-as-directory (concat sboo-root-directory
				 FilePath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-snippets-directory

  (sboo-subdir "snippets/")

  "Directory whose (per-major-mode) subdirectories contain my YASnippets files.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-theme-directory 

  (sboo-subdir "themes/")

  "Directory with themes (`.el' files which `provide-theme').")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-file (FilePath)
  "Return « `sboo-root-directory'/`FilePath' ».

  i.e. Return the relative filepath `FilePath', 
  as an absolute filepath, under `sboo-root-directory'.

  Calls `file-truename'.
  "

  (file-truename (concat sboo-root-directory
			 FilePath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-init-file

  (sboo-file "sboo-init.el")

  "Main configuration (like `user-init-file') for the `sboo'-profile.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-custom-file

  (sboo-file "sboo-custom.el")

  "Separate `custom-file' from `user-init-file'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-init-helm-file

  (sboo-file "sboo-init-helm.el")

  "`helm'-specific loading & configuration, for the `sboo'-profile. 
`helm` is an important package (i.e. it really should be installed), for erognomics 
(because it saves so much typing).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-init-use-package-file

  (sboo-file "sboo-init-use-package.el")

  "`use-package'-specific loading & configuration. 
`use-package` is an important package (i.e. it really should be installed), 
for further configuration of the `sboo'-profile. Why? 
Because, once installed (NOTE `use-package' may become a builtin), it loads & configures 
most other installed packages; concisely, efficiently, and safely.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-init-real-auto-save-file

  (sboo-file "sboo-init-real-auto-save.el")

  "`real-auto-save'-specific loading & configuration. 
`real-auto-save` is important (single-file) package (i.e. it really should be installed). Why? Because continuous (& convenient) autosaving saves you from lost work.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-package-file (PackageName &optional FileName)
  "Return « `sboo-submodule-directory'/`PackageName'/`FileName' ».

Calls `file-truename'.

e.g.:

(sboo-submodule-file \"real-auto-save\")
=
(sboo-submodule-file \"real-auto-save\" \"real-auto-save.el\")
=
\"/home/sboo/.emacs.d/submodules/real-auto-save/real-auto-save.el\"
"

  (let* ((PackageDirectory
          (truename-as-directory (concat sboo-vendored-package-directory
					 PackageName)))

	 (File (or FileName
	           (concat PackageName
			   ".el")))
	 
	 (PackageFile
	  (file-truename (concat PackageDirectory
				 File))))

    PackageFile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-register-submodule! (DirectoryName)

  "Register `DirectoryName' with `load-path', under `sboo-vendored-package-directory'.

See the file `./scripts/add-submodule.sh'."

  (let ((DirectoryPath 
         (truename-as-directory (concat sboo-vendored-package-directory
					DirectoryName))))

    (add-to-list 'load-path DirectoryPath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-load-file! (FileName)

  "`load' a `sboo-*.el' file."

  ;;TODO (interactive )

  (load (sboo-file FileName)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-require! (FeatureSymbol &optional NoError)

  "`require' a `sboo-*' feature."

  ;;TODO (interactive )

  (require FeatureSymbol nil NoError))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `file-truename':
;;
;; "file-truename handles ‘~’ in the same way that expand-file-name does."
;;
;; See:
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Truenames.html
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-definitions)