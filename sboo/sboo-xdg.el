

;;----------------------------------------------;;
;; Imports
;;----------------------------------------------;;

(require 'cl)

;;----------------------------------------------;;
;; Utilities
;;----------------------------------------------;;

(defun sboo-xdg-platform ()

  "The current platform, for XDG.

Output a symbol:

• 'posix   — for Linux and MacOs.
• 'windows — for Windows.
• 'posix   — for unknown filesystems.

The XDG Base Directories specification distinguishes between only these two platforms."

  (cond

   ((or (memq system-type   '(gnu/linux)))
    'posix)

   ((or (memq system-type   '(windows-nt ms-dos))
        (memq window-system '(w32 pc)))
    'windows)

   ((or (memq system-type   '(darwin))
        (memq window-system '(mac ns)))
    'posix)

   (t
    'posix)))

;;----------------------------------------------;;
;; Variables
;;----------------------------------------------;;

(defvar sboo-xdg-platform ;TODO defcustom

  (sboo-xdg-platform)

  "The current platform, for XDG.

See function `sboo-xdg-platform'.")

;;----------------------------------------------;;

(defconst xdg-data 'xdg-data
  "Symbol representing XDG_DATA_HOME.")

;;----------------------------------------------;;

(defconst xdg-config 'xdg-config
  "Symbol representing XDG_CONFIG_HOME.")

;;----------------------------------------------;;

(defconst xdg-cache 'xdg-cache
  "Symbol representing XDG_CACHE_HOME.")

;;----------------------------------------------;;

(defvar sboo-xdg-config-posix-default-value

  "$HOME/.config"

  "The default xdg-config directroy, on POSIX (Linux and Mac).

See function `sboo-xdg-config-posix-default-value'.

NOTE You must call `substitute-in-file-name' on this path to resolve the environment variable, \"$HOME\".")

;;----------------------------------------------;;

(defvar sboo-xdg-data-posix-default-value

  "$HOME/.local/share"

  "The default xdg-data directroy, on POSIX (Linux and Mac).

See function `sboo-xdg-data-posix-default-value'.

NOTE You must call `substitute-in-file-name' on this path to resolve the environment variable, \"$HOME\".")

;;----------------------------------------------;;

(defvar sboo-xdg-cache-posix-default-value

  "$HOME/.cache"

  "The default xdg-cache directroy, on POSIX (Linux and Mac).

See function `sboo-xdg-cache-posix-default-value'.

NOTE You must call `substitute-in-file-name' on this path to resolve the environment variable, \"$HOME\".")

;;----------------------------------------------;;

(defvar sboo-xdg-config-windows-default-value

  "C:\\Users\\%USERNAME%\\AppData\\Roaming"

  "The default xdg-config directroy, on Windows.

See function `sboo-xdg-config-windows-default-value'.

NOTE You must call `substitute-in-file-name' on this path to resolve the environment variable, \"%USERNAME%\".")

;;----------------------------------------------;;

(defvar sboo-xdg-data-windows-default-value

  "C:\\Users\\%USERNAME%\\AppData\\Roaming"

  "The default xdg-data directroy, on Windows.

See function `sboo-xdg-data-windows-default-value'.

NOTE You must call `substitute-in-file-name' on this path to resolve the environment variable, \"%USERNAME%\".")

;;----------------------------------------------;;

(defvar sboo-xdg-cache-windows-default-value

  "C:\\Users\\%USERNAME%\\AppData\\Local"

  "The default xdg-cache directroy, on Windows.

See function `sboo-xdg-cache-windows-default-value'.

NOTE You must call `substitute-in-file-name' on this path to resolve the environment variable, \"%USERNAME%\".")

;;----------------------------------------------;;

(defvar sboo-xdg-config-posix-environment-variable

  "XDG_CONFIG_HOME"

  "Environment variable for xdg-config, on POSIX (Linux and Mac).")

;;----------------------------------------------;;

(defvar sboo-xdg-data-posix-environment-variable

  "XDG_DATA_HOME"

  "Environment variable for xdg-data, on POSIX (Linux and Mac).")

;;----------------------------------------------;;

(defvar sboo-xdg-cache-posix-environment-variable

  "XDG_CACHE_HOME"

  "Environment variable for xdg-cache, on POSIX (Linux and Mac).")

;;----------------------------------------------;;

(defvar sboo-xdg-config-windows-environment-variable

  "APPDATA"

  "Environment variable for xdg-config, on Windows.")

;;----------------------------------------------;;

(defvar sboo-xdg-data-windows-environment-variable

  "APPDATA"

  "Environment variable for xdg-data, on Windows.")

;;----------------------------------------------;;

(defvar sboo-xdg-cache-windows-environment-variable

  "LOCALAPPDATA"

  "Environment variable for xdg-cache, on Windows.")

;;----------------------------------------------;;
;; Functions
;;----------------------------------------------;;

(defun sboo-xdg-config-by-default ()

  "The default XDG Config Base Directory.

sboo-xdg-config-by-default resolves either `sboo-xdg-config-posix-default-value' or `sboo-xdg-config-windows-default-value' 
(for the current platform)."

  (substitute-in-file-name

   (pcase sboo-xdg-platform

     ('posix   sboo-xdg-config-posix-default-value)

     ('windows sboo-xdg-config-windows-default-value)
     
     (_        sboo-xdg-config-posix-default-value))))

;;----------------------------------------------;;

(defun sboo-xdg-data-by-default ()

  "The default XDG Data Base Directory.

sboo-xdg-data-by-default resolves either `sboo-xdg-data-posix-default-value' or `sboo-xdg-data-windows-default-value' 
(for the current platform)."

  (substitute-in-file-name

   (pcase sboo-xdg-platform

     ('posix   sboo-xdg-data-posix-default-value)

     ('windows sboo-xdg-data-windows-default-value)
     
     (_        sboo-xdg-data-posix-default-value))))

;;----------------------------------------------;;

(defun sboo-xdg-cache-by-default ()

  "The default XDG Cache Base Directory.

sboo-xdg-cache-by-default resolves either `sboo-xdg-cache-posix-default-value' or `sboo-xdg-cache-windows-default-value' 
(for the current platform)."

  (substitute-in-file-name

   (pcase sboo-xdg-platform

     ('posix   sboo-xdg-cache-posix-default-value)

     ('windows sboo-xdg-cache-windows-default-value)
     
     (_        sboo-xdg-cache-posix-default-value))))

;;----------------------------------------------;;

(defun sboo-xdg-config-from-environment ()

  "The current XDG Config path Base Directory (if specified).

sboo-xdg-config-from-environment accesses either `sboo-xdg-config-posix-environment-variable' or `sboo-xdg-config-windows-environment-variable' 
(for the current platform)."

  (getenv

   (pcase sboo-xdg-platform

     ('posix   sboo-xdg-config-posix-environment-variable)

     ('windows sboo-xdg-config-windows-environment-variable)
     
     (_        sboo-xdg-config-posix-environment-variable))))

;;----------------------------------------------;;

(defun sboo-xdg-data-from-environment ()

  "The current XDG Data path Base Directory (if specified).

sboo-xdg-data-from-environment accesses either `sboo-xdg-data-posix-environment-variable' or `sboo-xdg-data-windows-environment-variable' 
(for the current platform)."

  (getenv

   (pcase sboo-xdg-platform

     ('posix   sboo-xdg-data-posix-environment-variable)

     ('windows sboo-xdg-data-windows-environment-variable)
     
     (_        sboo-xdg-data-posix-environment-variable))))

;;----------------------------------------------;;

(defun sboo-xdg-cache-from-environment ()

  "The current XDG Cache path Base Directory (if specified).

sboo-xdg-cache-from-environment accesses either `sboo-xdg-cache-posix-environment-variable' or `sboo-xdg-cache-windows-environment-variable' 
(for the current platform)."

  (getenv

   (pcase sboo-xdg-platform

     ('posix   sboo-xdg-cache-posix-environment-variable)

     ('windows sboo-xdg-cache-windows-environment-variable)
     
     (_        sboo-xdg-cache-posix-environment-variable))))

;;----------------------------------------------;;

(cl-defun sboo-xdg-config (path &key subdir)

  "Make an xdg path for config files, from PATH.

Inputs:

• PATH   — a string. the suffix of the filepath to output.
• SUBDIR — a string. if non-nil (or absent), prefix PATH with SUBDIR.

Output: a string. \"$XDG_CONFIG_HOME/SUBDIR/PATH\".

Example:

• M-: (sboo-xdg-config \"directory/basename.txt\")
      \"~/.config/directory/basename.txt\"

• M-: (sboo-xdg-config \"basename.txt\" :subdir \"directory\")
      \"~/.config/directory/basename.txt\"

The XDG Config Base Directory defaults to:

• on POSIX   — « ~/.config ».
• on Windows — « C:\\Users\\%USERNAME%\\AppData\\Roaming ».

Use for (e.g. ).

See URL `https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html'.

Related:

• `sboo-xdg-config-by-default'
• `sboo-xdg-config-from-environment'"

  (let* ((XdgBaseDirectoryEnvironment
          (sboo-xdg-config-from-environment))

         (XdgBaseDirectoryDefault
          (sboo-xdg-config-by-default))

         (XdgBaseDirectory
          (if XdgBaseDirectoryEnvironment
              XdgBaseDirectoryEnvironment
            XdgBaseDirectoryDefault))

         (Directory
          (if subdir
              (concat (file-name-as-directory XdgBaseDirectory) (file-name-as-directory subdir))
            (concat (file-name-as-directory XdgBaseDirectory))))
        )

    (file-truename
     (concat Directory path))))

;;----------------------------------------------;;

(cl-defun sboo-xdg-data (path &key subdir)

  "Make an xdg path for data files, from PATH.

Inputs:

• PATH   — a string. the suffix of the filepath to output.
• SUBDIR — a string. if non-nil (or absent), prefix PATH with SUBDIR.

Output: a string. \"$XDG_DATA_HOME/SUBDIR/PATH\".

Example:

• M-: (sboo-xdg-data \"directory/basename.txt\")
      \"~/.data/directory/basename.txt\"

• M-: (sboo-xdg-data \"basename.txt\" :subdir \"directory\")
      \"~/.data/directory/basename.txt\"

The XDG Data Base Directory defaults to:

• on POSIX   — « ~/.local/share ».
• on Windows — « C:\\Users\\%USERNAME%\\AppData\\Roaming ».

Use for (e.g. ).

See URL `https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html'.

Related:

• `sboo-xdg-data-by-default'
• `sboo-xdg-data-from-environment'"

  (let* ((XdgBaseDirectoryEnvironment
          (sboo-xdg-data-from-environment))

         (XdgBaseDirectoryDefault
          (sboo-xdg-data-by-default))

         (XdgBaseDirectory
          (if XdgBaseDirectoryEnvironment
              XdgBaseDirectoryEnvironment
            XdgBaseDirectoryDefault))

         (Directory
          (if subdir
              (concat (file-name-as-directory XdgBaseDirectory) (file-name-as-directory subdir))
            (concat (file-name-as-directory XdgBaseDirectory))))
        )

    (file-truename
     (concat Directory path))))

;;----------------------------------------------;;

(cl-defun sboo-xdg-cache (path &key subdir)

  "Make an xdg path for cache files, from PATH.

Inputs:

• PATH   — a string. the suffix of the filepath to output.
• SUBDIR — a string. if non-nil (or absent), prefix PATH with SUBDIR.

Output: a string. \"$XDG_CACHE_HOME/SUBDIR/PATH\".

Example:

• M-: (sboo-xdg-cache \"directory/basename.txt\")
      \"~/.cache/directory/basename.txt\"

• M-: (sboo-xdg-cache \"basename.txt\" :subdir \"directory\")
      \"~/.cache/directory/basename.txt\"

The XDG Cache Base Directory defaults to:

• on POSIX   — « ~/.cache ».
• on Windows — « C:\\Users\\%USERNAME%\\AppData\\Local ».

Use for (e.g. ).

See URL `https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html'.

Related:

• `sboo-xdg-cache-by-default'
• `sboo-xdg-cache-from-environment'"

  (let* ((XdgBaseDirectoryEnvironment
          (sboo-xdg-cache-from-environment))

         (XdgBaseDirectoryDefault
          (sboo-xdg-cache-by-default))

         (XdgBaseDirectory
          (if XdgBaseDirectoryEnvironment
              XdgBaseDirectoryEnvironment
            XdgBaseDirectoryDefault))

         (Directory
          (if subdir
              (concat (file-name-as-directory XdgBaseDirectory) (file-name-as-directory subdir))
            (concat (file-name-as-directory XdgBaseDirectory))))
        )

    (file-truename
     (concat Directory path))))

;;----------------------------------------------;;

;; XDG on Linux:
;;
;;     $ echo $XDG_CONFIG_HOME
;;     ~/.config
;;
;;     $ echo $XDG_DATA_HOME
;;     ~/.local/share
;;
;;     $ echo $XDG_CACHE_HOME
;;     ~/.cache
;;

;; XDG on Windows:
;;
;;     > echo %APPDATA%
;;     C:\Users\%USERNAME%\AppData\Roaming
;;
;;     > echo %LOCALAPPDATA%
;;     C:\Users\%USERNAME%\AppData\Local
;;

;;----------------------------------------------;;

(provide 'sboo-xdg)