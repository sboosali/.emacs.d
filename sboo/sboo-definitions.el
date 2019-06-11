;;; sboo-definitions.el --- Definitions for sboosali's configuration -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 10 Jun 2019
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Core definitions (no statements) for « sboo »'s personal configuration. 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile
  (require 'rx)
  (require 'pcase))

;;----------------------------------------------;;

(progn
  (require 'seq)
  (require 'cl-lib))

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defun truename-as-directory (FilePath)

  "Return « FILEPATH/ ».

Return the true name of FILEPATH, as a directory path:

* an absolute path, 
* with symbolic links resolved (but not hard links),
* with « ~/... » and « $HOME/... » expanded.

Calls `file-name-as-directory' and `file-truename'."

  (file-name-as-directory (file-truename FilePath)))

;;----------------------------------------------;;

(cl-defmacro sboo-assert! (form &key message) ;TODO;
  
  "Assert that FORM is t.

Related:

• `cl-assert'."

  (cl-assert form))

;;----------------------------------------------;;
;; Environment Variables -----------------------;;
;;----------------------------------------------;;

(defconst sboo-environment-variable-install "EMACS_INSTALL"

  "See `sboo-install-p'.

Example Usage: « $ EMACS_INSTALL=t emacs ».")

;;----------------------------------------------;;

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

;;----------------------------------------------;;

(defun sboo-install-submodules-p ()
  "Whether to load the `sboo-critical-packages' from vendored submodules.

Wraps `sboo-install-p'."

  (pcase (sboo-install-p)

      ('submodules   t)
      (_           nil)))

;;----------------------------------------------;;
;; Paths ---------------------------------------;;
;;----------------------------------------------;;

(defconst emacs-directory

  (truename-as-directory (or user-emacs-directory
			     "~/.emacs.d/"))

  "The root directory of the user's emacs configuration.")

;;----------------------------------------------;;

(defun emacs-file (FilePath)
  "Return « `emacs-directory'/`FilePath' ».

  i.e. Return the relative filepath `FilePath', 
  as an absolute filepath, under `emacs-directory'.

  Calls `file-truename'.
  "

  (file-truename (concat emacs-directory
			 FilePath)))

;;----------------------------------------------;;

(defun emacs-subdir (FilePath)
  "Return « `emacs-directory'/`FilePath'/ ».

  i.e. Return the relative directory `FilePath', 
  as an absolute sub-directory of `emacs-directory'.

  Calls `file-name-as-directory' and `file-truename'.
  "

  (truename-as-directory (concat emacs-directory
				 FilePath)))

;;----------------------------------------------;;

(defvar sboo-installed-package-directory

  (or (bound-and-true-p package-user-dir)
      (emacs-subdir "elpa/"))

  "Directory where `package.el' should install ELisp packages.
`package-user-dir' by default.")

;;----------------------------------------------;;

(defconst sboo-vendored-package-directory

  (emacs-subdir "submodules/")

  "Directory which contains any vendored ELisp packages (as subdirectories).")

;;----------------------------------------------;;

(defconst sboo-root-directory

  (emacs-subdir "sboo/")

  "The root directory of my personal configuration.")

;;----------------------------------------------;;

(defun sboo-subdir (FilePath)

  "Return « `sboo-root-directory'/FILEPATH/ ».

  i.e. Return the relative directory FILEPATH, 
  as an absolute sub-directory of `sboo-root-directory'.

  Calls `file-name-as-directory' and `file-truename'.
  "

  (truename-as-directory (concat sboo-root-directory FilePath)))

;;----------------------------------------------;;

(defconst sboo-data-directory

  (sboo-subdir "data/")

  "Directory with (miscellaneous) data files.")

;;----------------------------------------------;;

(defconst sboo-lisp-directory

  (sboo-subdir "lisp/")

  "Directory with vendored (individual) ELisp files.")

;;----------------------------------------------;;

(defconst sboo-theme-directory

  (sboo-subdir "themes/")

  "Directory with themes.

Themes are « .el » files which `provide-theme'.")

;;----------------------------------------------;;

(defconst sboo-icon-directory

  (sboo-subdir "icons/")

  "Directory with icons.

Icons are « .xpm » (or « .pbm ») files.")

;;----------------------------------------------;;

(defconst sboo-font-directory

  (sboo-subdir "font/")

  "Directory with Font files.")

;;----------------------------------------------;;

(defconst sboo-snippets-directory

  (sboo-subdir "snippets/")

  "Directory whose (per-major-mode) subdirectories contain snippets.

Snippets are « .yasnippet » template files.")

;;----------------------------------------------;;

(defun sboo-file (FilePath)

  "Return « `sboo-root-directory'/FILEPATH ».

i.e. Return the relative filepath FILEPATH, 
as an absolute filepath, under `sboo-root-directory'.

Calls `file-truename'."

  (file-truename (concat sboo-root-directory FilePath)))

;;----------------------------------------------;;

(defun sboo-data-file (FilePath)

  "Return « `sboo-data-directory'/FILEPATH ».

i.e. Return the relative filepath FILEPATH, 
as an absolute filepath, under `sboo-data-directory'.

Calls `file-truename'."

  (file-truename (concat sboo-data-directory FilePath)))

;;----------------------------------------------;;

(defun sboo-lisp-file (FilePath)

  "Return « `sboo-lisp-directory'/FILEPATH ».

i.e. Return the relative filepath FILEPATH, 
as an absolute filepath, under `sboo-lisp-directory'.

Calls `file-truename'."

  (file-truename (concat sboo-lisp-directory FilePath)))

;;----------------------------------------------;;

(defun sboo-theme-file (FilePath)

  "Return « `sboo-theme-directory'/FILEPATH ».

i.e. Return the relative filepath FILEPATH, 
as an absolute filepath, under `sboo-theme-directory'.

Calls `file-truename'."

  (file-truename (concat sboo-theme-directory FilePath)))

;;----------------------------------------------;;

(defun sboo-icon-file (FilePath)

  "Return « `sboo-icon-directory'/FILEPATH ».

i.e. Return the relative filepath FILEPATH, 
as an absolute filepath, under `sboo-icon-directory'.

Calls `file-truename'."

  (file-truename (concat sboo-icon-directory FilePath)))

;;----------------------------------------------;;

(defun sboo-font-file (FilePath)

  "Return « `sboo-font-directory'/FILEPATH ».

i.e. Return the relative filepath FILEPATH, 
as an absolute filepath, under `sboo-font-directory'.

Calls `file-truename'."

  (file-truename (concat sboo-font-directory FilePath)))

;;----------------------------------------------;;

(defconst sboo-init-file

  (sboo-file "sboo-init.el")

  "Main configuration (like `user-init-file') for the `sboo'-profile.")

;;----------------------------------------------;;

(defconst sboo-custom-file

  (sboo-file "sboo-custom.el")

  "Separate `custom-file' from `user-init-file'.")

;;----------------------------------------------;;

(defconst sboo-init-helm-file

  (sboo-file "sboo-init-helm.el")

  "`helm'-specific loading & configuration, for the `sboo'-profile. 
`helm` is an important package (i.e. it really should be installed), for erognomics 
(because it saves so much typing).")

;;----------------------------------------------;;

(defconst sboo-init-use-package-file

  (sboo-file "sboo-init-use-package.el")

  "`use-package'-specific loading & configuration. 
`use-package` is an important package (i.e. it really should be installed), 
for further configuration of the `sboo'-profile. Why? 
Because, once installed (NOTE `use-package' may become a builtin), it loads & configures 
most other installed packages; concisely, efficiently, and safely.")

;;----------------------------------------------;;

(defconst sboo-init-real-auto-save-file

  (sboo-file "sboo-init-real-auto-save.el")

  "`real-auto-save'-specific loading & configuration. 
`real-auto-save` is important (single-file) package (i.e. it really should be installed). Why? Because continuous (& convenient) autosaving saves you from lost work.")

;;----------------------------------------------;;

(defun sboo-submodule-file (package-name &optional file-name)

  "Return « \"`sboo-submodule-directory'/`PACKAGE-NAME'/`FILE-NAME'\" ».

Calls `file-truename'.

Examples:

• M-: (sboo-submodule-file \"dante\")
    ⇒ (sboo-submodule-file \"dante\" \"dante.el\")
    ⇒ \"/home/sboo/.emacs.d/submodules/dante/dante.el\"

Related:

• `sboo-submodule-directory'.

Links:

• URL `https://git-scm.com/book/en/v2/Git-Tools-Submodules'.
• URL `https://chrisjean.com/git-submodules-adding-using-removing-and-updating/'."

  (let* ((PackageDirectory (truename-as-directory
                            (concat sboo-vendored-package-directory package-name)))

	 (File (or file-name
	           (concat package-name ".el")))
 
	 (PackageFile (file-truename
                       (concat PackageDirectory File)))
         )

    PackageFile))

;;----------------------------------------------;;

(cl-defun sboo-submodule-directory (package-name &key version)

  "Return « \"`sboo-submodule-directory'/`PACKAGE-NAME-VERSION'\" ».

Examples:

• M-: (add-to-list 'load-path (sboo-submodule-directory \"use-package\")
    ⇒ \"/home/sboo/.emacs.d/submodules/use-package/\"

• M-: (add-to-list 'load-path (sboo-submodule-directory \"use-package\" :version \"2.3\")
    ⇒ \"/home/sboo/.emacs.d/submodules/use-package-2.3/\"

Links:

• URL `https://git-scm.com/book/en/v2/Git-Tools-Submodules'.
• URL `https://chrisjean.com/git-submodules-adding-using-removing-and-updating/'."

  (let* ((PackageDirectoryPrefix (concat sboo-vendored-package-directory package-name))

	 (VersionSuffix          (if version
	                             (format "-%s" version)
                                   ""))

	 (PackageDirectory (file-name-as-directory
                            (concat PackageDirectoryPrefix VersionSuffix)))
         )

    PackageDirectory))

;;----------------------------------------------;;

(defun sboo-register-submodule-packages! (SubmoduleDirectory)

  "Register `SubmoduleDirectory' with `load-path', under `sboo-vendored-package-directory'.

See the file `./scripts/add-submodule.sh'."

  (let ((DirectoryPath 
         (truename-as-directory (concat sboo-vendored-package-directory
					SubmoduleDirectory)))
         )

    (progn
      (add-to-list 'load-path DirectoryPath)
      DirectoryPath)))

;;----------------------------------------------;;

(defun sboo-register-submodule-themes! (SubmoduleDirectory)

  "Register `SubmoduleDirectory' with `custom-theme-load-path', under `sboo-vendored-package-directory'.

See the file `./scripts/add-submodule.sh'."

  (let ((DirectoryPath 
         (truename-as-directory (concat sboo-vendored-package-directory
					SubmoduleDirectory)))
        )

    (progn
      (add-to-list 'custom-theme-load-path DirectoryPath)
      DirectoryPath)))

;;----------------------------------------------;;

(defun sboo-load-file! (FileName)

  "`load' a `sboo-*.el' file."

  ;;TODO (interactive )

  (load (sboo-file FileName)))

;;----------------------------------------------;;

(defun add-to-load-path! (directory)

  "Register DIRECTORY (a directory containing Elisp `provide's) with `load-path'.

DIRECTORY:

• /must/ be an absolute filepath to a directory; (TODO)

• /should/ use forward-slashes, e.g. `.../.../...'
  (they're automatically converted to the platform-specifc directory-separator character);

• /may/ start with `~/' 
  (tildes are expanded to the user's home directory);

• /may/ end with a forward-slash (e.g. `sboo/' or `sboo')
  (a trailing is added if absent)."

  (interactive (list
                (read-directory-name "ELisp Directory: " )))

  (let* ((DIRECTORY (file-name-as-directory (file-truename directory)))
         )
    (add-to-list 'load-path DIRECTORY)))

;;----------------------------------------------;;

(defun add-to-theme-path! (directory)

  "Register DIRECTORY (a directory containing Emacs `provide-theme's) with `custom-theme-load-path'.

Related:

• `add-to-load-path!' — (See for details on the DIRECTORY argument.)"

  (interactive (list
                (read-directory-name "Theme Directory: " )))

  (let* ((DIRECTORY (file-name-as-directory (file-truename directory)))
         )
    (add-to-list 'custom-theme-load-path DIRECTORY)))

;;----------------------------------------------;;

(defun add-to-exec-path! (directory)

  "Register DIRECTORY (a directory containing executables) with `exec-path'.

Related:

• `add-to-load-path!' — (See for details on the DIRECTORY argument.)"

  (interactive (list
                (read-directory-name "Program Directory: " )))

  (let* ((DIRECTORY (file-name-as-directory (file-truename directory)))
         )
    (add-to-list 'exec-path DIRECTORY)))

;;----------------------------------------------;;

(defun add-to-icon-path! (directory)

  "Register DIRECTORY (a directory containing bitmap images) with `image-load-path'.

For example, « .../share/emacs/26.1/etc/images/ » contains:

• « undo.xpm » — an icon in the « X PixMap » (XPM) image file format.
• « undo.pbm » — an icon in the « Portable Bitmap Format » (PBM) image file format.

Links:

• URL `https://en.wikipedia.org/wiki/X_PixMap'
• URL `https://en.wikipedia.org/wiki/Netpbm_format'

Related:

• `add-to-load-path!' — (See for details on the DIRECTORY argument.)"

  (interactive (list
                (read-directory-name "Icon Directory: " )))

  (let* ((DIRECTORY (file-name-as-directory (file-truename directory)))
         )
    (add-to-list 'image-load-path DIRECTORY)))

;;----------------------------------------------;;

(defun add-startup-hook! (function-symbol)

  "Register FUNCTION-SYMBOL with `emacs-startup-hook'."

  (add-hook 'emacs-startup-hook function-symbol))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; `file-truename':
;;
;; "file-truename handles ‘~’ in the same way that ‘expand-file-name’ does."
;;

;; Links:
;;
;; • URL `https://www.gnu.org/software/emacs/manual/html_node/elisp/Truenames.html'
;; • URL `'
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-definitions)

;;; sboo-definitions.el ends here