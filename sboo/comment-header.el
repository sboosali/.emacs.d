;;; comment-header.el --- Insert (padded) headers within comments.
;;; -*- lexical-binding: t; -*-

;;==============================================;;
;; Copyright (C) GPL-3.0-or-later 2019 Spiros Boosalis

;; Author: Spiros Boosalis <samboosalis@gmail.com> 
;; Maintainer: Spiros Boosalis <samboosalis@gmail.com> 
;; Version: 0.0.0
;; URL: https://github.com/sboosali/comment-header
;; Keywords:
;; Package-Requires: ((cl-lib "0.5") (pkg-info "0.4") (emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;;==============================================;;
;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;==============================================;;
;;; Commentary:

;; GNU Emacs major mode for Insert (padded) headers within comments..
;;
;; Usage: 
;;
;;   (progn (require 'comment-header) (comment-header-help))
;;
;; Installation: 
;;
;;   (use-package comment-header)
;;
;; Commands provided:
;;
;; • `comment-header-help'
;; • `comment-header-version'
;;
;; Variables provided:
;;
;; • `comment-header-default'
;;
;; Bugs: https://github.com/sboosali/comment-header/issues
;;
;; History: https://github.com/sboosali/comment-header/blob/master/CHANGELOG.md
;;

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;;; Requirements -------------------------------;;
;;----------------------------------------------;;

(require 'cl-lib)
(require 'pcase)
(require 'seq)
(require 'subr-x)
(require 'pkg-info)

;;----------------------------------------------;;

(eval-when-compile
  (require 'rx))

;;----------------------------------------------;;
;;; Customization ------------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defgroup comment-header nil

  "Insert (padded) headers within comments.."

  :link '(url-link :tag "GitHub" "https://github.com/sboosali/comment-header#readme")

  :prefix "comment-header-"
  ;;  :group '
  )

;;==============================================;;

(defcustom comment-header-default

  nil

  "Default.

a `symbolp'."

  :type '(radio (const nil :tag "Disable")
                (const t   :tag "Enable "))

  :safe #'symbolp
  :group 'comment-header)

;;----------------------------------------------;;
;;; Commands -----------------------------------;;
;;----------------------------------------------;;

(defun comment-header-help ()

  "."

  (interactive)

  (let* (
         )

    ()))

;;----------------------------------------------;;

(defun comment-header-version (&optional show-version)

  "Display string describing the version of BNF Mode.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the Echo Area and the Messages Buffer.

The returned string includes both, the version from package.el
and the library version, if both are present and different.

If the version number could not be determined, signal an error
if called interactively or if SHOW-VERSION is non-nil, otherwise
just return nil."

  (interactive (list t))

  (let ((VERSION (pkg-info-version-info 'comment-header))
        )

    (when show-version
      (message "`comment-header''s version = %s" VERSION))
    VERSION))

;;----------------------------------------------;;
;; Notes: --------------------------------------;;
;;----------------------------------------------;;

;; `upcase-initials':
;;
;; M-: (upcase-initials "The CAT in the hAt")
;;                    ⇒ "The CAT In The HAt"
;;

;;==============================================;;
(provide 'comment-header)

;; Local Variables:
;; End:

;;; comment-header.el ends here























;;; comment-header.el --- Insert padded comment headers
;;; -*- lexical-binding: t; -*-

;;==============================================;;
;; Copyright (C) GPL-3.0-or-later 2019 Spiros Boosalis

;; Author: Spiros Boosalis <samboosalis@gmail.com> 
;; Maintainer: Spiros Boosalis <samboosalis@gmail.com> 
;; Version: 0.0.0
;; URL: https://github.com/sboosali/comment-header.el
;; Keywords:
;; Package-Requires: ((cl-lib "0.5") (pkg-info "0.4") (emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;;==============================================;;
;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;==============================================;;
;;; Commentary:

;; GNU Emacs major mode for .
;;
;; 
;;
;; Usage: 
;;
;;   (require 'comment-header)
;;
;; Bugs: https://github.com/sboosali/comment-header.el/issues
;;
;; History: https://github.com/sboosali/comment-header.el/blob/master/CHANGELOG.md

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;;; Compatibility
;;----------------------------------------------;;

;; Work around emacs bug#18845, cc-mode expects cl to be loaded
;; while bnf-mode only uses cl-lib (without compatibility aliases)

(eval-and-compile
  (if (and (= emacs-major-version 24) (>= emacs-minor-version 4))
      (require 'cl-lib)))

;;----------------------------------------------;;
;;; Requirements
;;----------------------------------------------;;




;;; YY --- Major mode for SYNOPSIS -*- lexical-binding: t; -*-

;;==============================================;;
;; Copyright (C) 2019 Spiros Boosalis

;; Author: Spiros Boosalis <samboosalis@gmail.com> 
;; Maintainer: Spiros Boosalis <samboosalis@gmail.com> 
;; Version: 0.1.0
;; URL: https://github.com/sboosali/YY
;; Keywords: languages
;; Package-Requires: ((cl-lib "0.5") (pkg-info "0.4") (emacs "24.3"))

;; This file is not part of GNU Emacs.

;;==============================================;;
;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;==============================================;;
;;; Commentary:

;; GNU Emacs major mode for .
;;
;; 
;;
;; Usage: 
;;
;;   (require 'bnf-mode)
;;
;; Bugs: https://github.com/sboosali/YY/issues
;;
;; History: https://github.com/sboosali/YY/blob/master/CHANGELOG.md

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;;; Compatibility
;;----------------------------------------------;;

;; Work around emacs bug#18845, cc-mode expects cl to be loaded
;; while bnf-mode only uses cl-lib (without compatibility aliases)

(eval-and-compile
  (if (and (= emacs-major-version 24) (>= emacs-minor-version 4))
      (require 'cl-lib)))

;;----------------------------------------------;;
;;; Requirements
;;----------------------------------------------;;

;; Tell the byte compiler about autoloaded functions from packages
(declare-function pkg-info-version-info "pkg-info" (package))

;;----------------------------------------------;;

(eval-when-compile
  (require 'rx))

;;----------------------------------------------;;

(require 'cl-lib)
(require 'pkg-info)

;;==============================================;;
;;; Commentary:

;; Insert (padded, aligned) comment headers/sections.
;; 
;; • 
;; • 
;;
;; `provide'd Commands include:
;;
;; • `comment-header-insert-h1'
;; • `comment-header-insert-h2'
;; • `comment-header-insert-header'
;; 
;; `provide'd Variables (which are customizeable) include:
;;
;; • `comment-header-character-default'
;; • `comment-header-length-default'
;; • `comment-header-prefix-string-alist'
;; • `comment-header-infix-character-alist'
;; • `comment-header-suffix-string-alist'
;;
;; Related Variables (builtin to Emacs) include:
;;
;; • `comment-start'
;;

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(require 'cl-lib)
(require 'pcase)
(require 'seq)

;;----------------------------------------------;;
;; Types ---------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;; 
;; 
;; 
;;==============================================;;
(provide 'comment-header)

;; Local Variables:
;; End:

;;; comment-header.el ends here