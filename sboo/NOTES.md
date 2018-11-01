# Notes on `Emacs`, `elisp`, packages, modes.

## `bind-key`

```elisp
;; M-x pp-macroexpand-last-sexp
;;
;; (bind-key "<kp-1>" sboo-company-complete-1 company-active-map nil))
;;
;; →

(let*
    ((name "<kp-1>")
     (key
      (if
          (vectorp name)
          name
        (read-kbd-macro name)))
     (kdesc
      (cons
       (if
           (stringp name)
           name
         (key-description name))
       'company-active-map))
     (binding
      (lookup-key
       (or company-active-map global-map)
       key)))
  (add-to-list 'personal-keybindings
               (list kdesc 'sboo-company-complete-1
                     (unless
                         (numberp binding)
                       binding)))
  (define-key
    (or company-active-map global-map)
    key 'sboo-company-complete-1))
```

## `dired`

### `dired-mode-map`

```elisp
(defvar dired-mode-map

  ;; This looks ugly when substitute-command-keys uses C-d instead d:
  ;;  (define-key dired-mode-map "\C-d" 'dired-flag-file-deletion)

  (let ((map (make-keymap)))

    (set-keymap-parent map special-mode-map)

    (define-key map [mouse-2] 'dired-mouse-find-file-other-window)
    (define-key map [follow-link] 'mouse-face)

    ;; Commands to mark or flag certain categories of files

    (define-key map "#" 'dired-flag-auto-save-files)
    (define-key map "." 'dired-clean-directory)
    (define-key map "~" 'dired-flag-backup-files)

    ;; Upper case keys (except !) for operating on the marked files

    (define-key map "A" 'dired-do-find-regexp)
    (define-key map "C" 'dired-do-copy)
    (define-key map "B" 'dired-do-byte-compile)
    (define-key map "D" 'dired-do-delete)
    (define-key map "G" 'dired-do-chgrp)
    (define-key map "H" 'dired-do-hardlink)
    (define-key map "L" 'dired-do-load)
    (define-key map "M" 'dired-do-chmod)
    (define-key map "O" 'dired-do-chown)
    (define-key map "P" 'dired-do-print)
    (define-key map "Q" 'dired-do-find-regexp-and-replace)
    (define-key map "R" 'dired-do-rename)
    (define-key map "S" 'dired-do-symlink)
    (define-key map "T" 'dired-do-touch)
    (define-key map "X" 'dired-do-shell-command)
    (define-key map "Z" 'dired-do-compress)
    (define-key map "c" 'dired-do-compress-to)
    (define-key map "!" 'dired-do-shell-command)
    (define-key map "&" 'dired-do-async-shell-command)

    ;; Comparison commands

    (define-key map "=" 'dired-diff)

    ;; Tree Dired commands

    (define-key map "\M-\C-?" 'dired-unmark-all-files)
    (define-key map "\M-\C-d" 'dired-tree-down)
    (define-key map "\M-\C-u" 'dired-tree-up)
    (define-key map "\M-\C-n" 'dired-next-subdir)
    (define-key map "\M-\C-p" 'dired-prev-subdir)

    ;; move to marked files

    (define-key map "\M-{" 'dired-prev-marked-file)
    (define-key map "\M-}" 'dired-next-marked-file)

    ;; Make all regexp commands share a `%' prefix:
    ;; We used to get to the submap via a symbol dired-regexp-prefix,
    ;; but that seems to serve little purpose, and copy-keymap
    ;; does a better job without it.

    (define-key map "%" nil)
    (define-key map "%u" 'dired-upcase)
    (define-key map "%l" 'dired-downcase)
    (define-key map "%d" 'dired-flag-files-regexp)
    (define-key map "%g" 'dired-mark-files-containing-regexp)
    (define-key map "%m" 'dired-mark-files-regexp)
    (define-key map "%r" 'dired-do-rename-regexp)
    (define-key map "%C" 'dired-do-copy-regexp)
    (define-key map "%H" 'dired-do-hardlink-regexp)
    (define-key map "%R" 'dired-do-rename-regexp)
    (define-key map "%S" 'dired-do-symlink-regexp)
    (define-key map "%&" 'dired-flag-garbage-files)

    ;; Commands for marking and unmarking.

    (define-key map "*" nil)
    (define-key map "**" 'dired-mark-executables)
    (define-key map "*/" 'dired-mark-directories)
    (define-key map "*@" 'dired-mark-symlinks)
    (define-key map "*%" 'dired-mark-files-regexp)
    (define-key map "*c" 'dired-change-marks)
    (define-key map "*s" 'dired-mark-subdir-files)
    (define-key map "*m" 'dired-mark)
    (define-key map "*u" 'dired-unmark)
    (define-key map "*?" 'dired-unmark-all-files)
    (define-key map "*!" 'dired-unmark-all-marks)
    (define-key map "U" 'dired-unmark-all-marks)
    (define-key map "*\177" 'dired-unmark-backward)
    (define-key map "*\C-n" 'dired-next-marked-file)
    (define-key map "*\C-p" 'dired-prev-marked-file)
    (define-key map "*t" 'dired-toggle-marks)

    ;; Lower keys for commands not operating on all the marked files

    (define-key map "a" 'dired-find-alternate-file)
    (define-key map "d" 'dired-flag-file-deletion)
    (define-key map "e" 'dired-find-file)
    (define-key map "f" 'dired-find-file)
    (define-key map "\C-m" 'dired-find-file)

    (put 'dired-find-file :advertised-binding "\C-m")

    (define-key map "g" 'revert-buffer)
    (define-key map "i" 'dired-maybe-insert-subdir)
    (define-key map "j" 'dired-goto-file)
    (define-key map "k" 'dired-do-kill-lines)
    (define-key map "l" 'dired-do-redisplay)
    (define-key map "m" 'dired-mark)
    (define-key map "n" 'dired-next-line)
    (define-key map "o" 'dired-find-file-other-window)
    (define-key map "\C-o" 'dired-display-file)
    (define-key map "p" 'dired-previous-line)
    (define-key map "s" 'dired-sort-toggle-or-edit)
    (define-key map "t" 'dired-toggle-marks)
    (define-key map "u" 'dired-unmark)
    (define-key map "v" 'dired-view-file)
    (define-key map "w" 'dired-copy-filename-as-kill)
    (define-key map "W" 'browse-url-of-dired-file)
    (define-key map "x" 'dired-do-flagged-delete)
    (define-key map "y" 'dired-show-file-type)
    (define-key map "+" 'dired-create-directory)

    ;; moving

    (define-key map "<" 'dired-prev-dirline)
    (define-key map ">" 'dired-next-dirline)
    (define-key map "^" 'dired-up-directory)
    (define-key map " " 'dired-next-line)

    (define-key map [?\S-\ ] 'dired-previous-line)
    (define-key map [remap next-line] 'dired-next-line)
    (define-key map [remap previous-line] 'dired-previous-line)

    ;; hiding

    (define-key map "$" 'dired-hide-subdir)
    (define-key map "\M-$" 'dired-hide-all)
    (define-key map "(" 'dired-hide-details-mode)

    ;; isearch

    (define-key map (kbd "M-s a C-s")   'dired-do-isearch)
    (define-key map (kbd "M-s a M-C-s") 'dired-do-isearch-regexp)
    (define-key map (kbd "M-s f C-s")   'dired-isearch-filenames)
    (define-key map (kbd "M-s f M-C-s") 'dired-isearch-filenames-regexp)

    ;; misc

    (define-key map [remap read-only-mode] 'dired-toggle-read-only)
    ;; `toggle-read-only' is an obsolete alias for `read-only-mode'

    (define-key map [remap toggle-read-only] 'dired-toggle-read-only)
    (define-key map "?" 'dired-summary)
    (define-key map "\177" 'dired-unmark-backward)
    (define-key map [remap undo] 'dired-undo)
    (define-key map [remap advertised-undo] 'dired-undo)

    ;; thumbnail manipulation (image-dired)

    (define-key map "\C-td" 'image-dired-display-thumbs)
    (define-key map "\C-tt" 'image-dired-tag-files)
    (define-key map "\C-tr" 'image-dired-delete-tag)
    (define-key map "\C-tj" 'image-dired-jump-thumbnail-buffer)
    (define-key map "\C-ti" 'image-dired-dired-display-image)
    (define-key map "\C-tx" 'image-dired-dired-display-external)
    (define-key map "\C-ta" 'image-dired-display-thumbs-append)
    (define-key map "\C-t." 'image-dired-display-thumb)
    (define-key map "\C-tc" 'image-dired-dired-comment-files)
    (define-key map "\C-tf" 'image-dired-mark-tagged-files)
    (define-key map "\C-t\C-t" 'image-dired-dired-toggle-marked-thumbs)
    (define-key map "\C-te" 'image-dired-dired-edit-comment-and-tags)

    ;; encryption and decryption (epa-dired)

    (define-key map ":d" 'epa-dired-do-decrypt)
    (define-key map ":v" 'epa-dired-do-verify)
    (define-key map ":s" 'epa-dired-do-sign)
    (define-key map ":e" 'epa-dired-do-encrypt)

    ;; Make menu bar items.

    ;; No need to fo this, now that top-level items are fewer.
    ;;;;
    ;; Get rid of the Edit menu bar item to save space.
    ;(define-key map [menu-bar edit] 'undefined)

    (define-key map [menu-bar subdir]
      (cons "Subdir" (make-sparse-keymap "Subdir")))

    (define-key map [menu-bar subdir hide-all]
      '(menu-item "Hide All" dired-hide-all
		  :help "Hide all subdirectories, leave only header lines"))
    (define-key map [menu-bar subdir hide-subdir]
      '(menu-item "Hide/UnHide Subdir" dired-hide-subdir
		  :help "Hide or unhide current directory listing"))
    (define-key map [menu-bar subdir tree-down]
      '(menu-item "Tree Down" dired-tree-down
		  :help "Go to first subdirectory header down the tree"))
    (define-key map [menu-bar subdir tree-up]
      '(menu-item "Tree Up" dired-tree-up
		  :help "Go to first subdirectory header up the tree"))
    (define-key map [menu-bar subdir up]
      '(menu-item "Up Directory" dired-up-directory
		  :help "Edit the parent directory"))
    (define-key map [menu-bar subdir prev-subdir]
      '(menu-item "Prev Subdir" dired-prev-subdir
		  :help "Go to previous subdirectory header line"))
    (define-key map [menu-bar subdir next-subdir]
      '(menu-item "Next Subdir" dired-next-subdir
		  :help "Go to next subdirectory header line"))
    (define-key map [menu-bar subdir prev-dirline]
      '(menu-item "Prev Dirline" dired-prev-dirline
		  :help "Move to next directory-file line"))
    (define-key map [menu-bar subdir next-dirline]
      '(menu-item "Next Dirline" dired-next-dirline
		  :help "Move to previous directory-file line"))
    (define-key map [menu-bar subdir insert]
      '(menu-item "Insert This Subdir" dired-maybe-insert-subdir
		  :help "Insert contents of subdirectory"
		  :enable (let ((f (dired-get-filename nil t)))
			    (and f (file-directory-p f)))))
    (define-key map [menu-bar immediate]
      (cons "Immediate" (make-sparse-keymap "Immediate")))

    (define-key map
      [menu-bar immediate image-dired-dired-display-external]
      '(menu-item "Display Image Externally" image-dired-dired-display-external
                  :help "Display image in external viewer"))
    (define-key map
      [menu-bar immediate image-dired-dired-display-image]
      '(menu-item "Display Image" image-dired-dired-display-image
                  :help "Display sized image in a separate window"))
    (define-key map
      [menu-bar immediate image-dired-dired-toggle-marked-thumbs]
      '(menu-item "Toggle Image Thumbnails in This Buffer" image-dired-dired-toggle-marked-thumbs
                  :help "Add or remove image thumbnails in front of marked file names"))

    (define-key map [menu-bar immediate hide-details]
      '(menu-item "Hide Details" dired-hide-details-mode
		  :help "Hide details in buffer"
		  :button (:toggle . dired-hide-details-mode)))
    (define-key map [menu-bar immediate revert-buffer]
      '(menu-item "Refresh" revert-buffer
		  :help "Update contents of shown directories"))

    (define-key map [menu-bar immediate dashes]
      '("--"))

    (define-key map [menu-bar immediate isearch-filenames-regexp]
      '(menu-item "Isearch Regexp in File Names..." dired-isearch-filenames-regexp
		  :help "Incrementally search for regexp in file names only"))
    (define-key map [menu-bar immediate isearch-filenames]
      '(menu-item "Isearch in File Names..." dired-isearch-filenames
		  :help "Incrementally search for string in file names only."))
    (define-key map [menu-bar immediate compare-directories]
      '(menu-item "Compare Directories..." dired-compare-directories
		  :help "Mark files with different attributes in two Dired buffers"))
    (define-key map [menu-bar immediate backup-diff]
      '(menu-item "Compare with Backup" dired-backup-diff
		  :help "Diff file at cursor with its latest backup"))
    (define-key map [menu-bar immediate diff]
      '(menu-item "Diff..." dired-diff
		  :help "Compare file at cursor with another file"))
    (define-key map [menu-bar immediate view]
      '(menu-item "View This File" dired-view-file
		  :help "Examine file at cursor in read-only mode"))
    (define-key map [menu-bar immediate display]
      '(menu-item "Display in Other Window" dired-display-file
		  :help "Display file at cursor in other window"))
    (define-key map [menu-bar immediate find-file-other-window]
      '(menu-item "Find in Other Window" dired-find-file-other-window
		  :help "Edit file at cursor in other window"))
    (define-key map [menu-bar immediate find-file]
      '(menu-item "Find This File" dired-find-file
		  :help "Edit file at cursor"))
    (define-key map [menu-bar immediate create-directory]
      '(menu-item "Create Directory..." dired-create-directory
		  :help "Create a directory"))
    (define-key map [menu-bar immediate wdired-mode]
      '(menu-item "Edit File Names" wdired-change-to-wdired-mode
		  :help "Put a Dired buffer in a mode in which filenames are editable"
		  :keys "C-x C-q"
		  :filter (lambda (x) (if (eq major-mode 'dired-mode) x))))

    (define-key map [menu-bar regexp]
      (cons "Regexp" (make-sparse-keymap "Regexp")))

    (define-key map
      [menu-bar regexp image-dired-mark-tagged-files]
      '(menu-item "Mark From Image Tag..." image-dired-mark-tagged-files
                  :help "Mark files whose image tags matches regexp"))

    (define-key map [menu-bar regexp dashes-1]
      '("--"))

    (define-key map [menu-bar regexp downcase]
      '(menu-item "Downcase" dired-downcase
		  ;; When running on plain MS-DOS, there's only one
		  ;; letter-case for file names.
		  :enable (or (not (fboundp 'msdos-long-file-names))
			      (msdos-long-file-names))
		  :help "Rename marked files to lower-case name"))
    (define-key map [menu-bar regexp upcase]
      '(menu-item "Upcase" dired-upcase
		  :enable (or (not (fboundp 'msdos-long-file-names))
			      (msdos-long-file-names))
		  :help "Rename marked files to upper-case name"))
    (define-key map [menu-bar regexp hardlink]
      '(menu-item "Hardlink..." dired-do-hardlink-regexp
		  :help "Make hard links for files matching regexp"))
    (define-key map [menu-bar regexp symlink]
      '(menu-item "Symlink..." dired-do-symlink-regexp
		  :visible (fboundp 'make-symbolic-link)
		  :help "Make symbolic links for files matching regexp"))
    (define-key map [menu-bar regexp rename]
      '(menu-item "Rename..." dired-do-rename-regexp
		  :help "Rename marked files matching regexp"))
    (define-key map [menu-bar regexp copy]
      '(menu-item "Copy..." dired-do-copy-regexp
		  :help "Copy marked files matching regexp"))
    (define-key map [menu-bar regexp flag]
      '(menu-item "Flag..." dired-flag-files-regexp
		  :help "Flag files matching regexp for deletion"))
    (define-key map [menu-bar regexp mark]
      '(menu-item "Mark..." dired-mark-files-regexp
		  :help "Mark files matching regexp for future operations"))
    (define-key map [menu-bar regexp mark-cont]
      '(menu-item "Mark Containing..." dired-mark-files-containing-regexp
		  :help "Mark files whose contents matches regexp"))

    (define-key map [menu-bar mark]
      (cons "Mark" (make-sparse-keymap "Mark")))

    (define-key map [menu-bar mark prev]
      '(menu-item "Previous Marked" dired-prev-marked-file
		  :help "Move to previous marked file"))
    (define-key map [menu-bar mark next]
      '(menu-item "Next Marked" dired-next-marked-file
		  :help "Move to next marked file"))
    (define-key map [menu-bar mark marks]
      '(menu-item "Change Marks..." dired-change-marks
		  :help "Replace marker with another character"))
    (define-key map [menu-bar mark unmark-all]
      '(menu-item "Unmark All" dired-unmark-all-marks))
    (define-key map [menu-bar mark symlinks]
      '(menu-item "Mark Symlinks" dired-mark-symlinks
		  :visible (fboundp 'make-symbolic-link)
		  :help "Mark all symbolic links"))
    (define-key map [menu-bar mark directories]
      '(menu-item "Mark Directories" dired-mark-directories
		  :help "Mark all directories except `.' and `..'"))
    (define-key map [menu-bar mark directory]
      '(menu-item "Mark Old Backups" dired-clean-directory
		  :help "Flag old numbered backups for deletion"))
    (define-key map [menu-bar mark executables]
      '(menu-item "Mark Executables" dired-mark-executables
		  :help "Mark all executable files"))
    (define-key map [menu-bar mark garbage-files]
      '(menu-item "Flag Garbage Files" dired-flag-garbage-files
		  :help "Flag unneeded files for deletion"))
    (define-key map [menu-bar mark backup-files]
      '(menu-item "Flag Backup Files" dired-flag-backup-files
		  :help "Flag all backup files for deletion"))
    (define-key map [menu-bar mark auto-save-files]
      '(menu-item "Flag Auto-save Files" dired-flag-auto-save-files
		  :help "Flag auto-save files for deletion"))
    (define-key map [menu-bar mark deletion]
      '(menu-item "Flag" dired-flag-file-deletion
		  :help "Flag current line's file for deletion"))
    (define-key map [menu-bar mark unmark]
      '(menu-item "Unmark" dired-unmark
		  :help "Unmark or unflag current line's file"))
    (define-key map [menu-bar mark mark]
      '(menu-item "Mark" dired-mark
		  :help "Mark current line's file for future operations"))
    (define-key map [menu-bar mark toggle-marks]
      '(menu-item "Toggle Marks" dired-toggle-marks
		  :help "Mark unmarked files, unmark marked ones"))

    (define-key map [menu-bar operate]
      (cons "Operate" (make-sparse-keymap "Operate")))

    (define-key map
      [menu-bar operate image-dired-delete-tag]
      '(menu-item "Delete Image Tag..." image-dired-delete-tag
                  :help "Delete image tag from current or marked files"))
    (define-key map
      [menu-bar operate image-dired-tag-files]
      '(menu-item "Add Image Tags..." image-dired-tag-files
                  :help "Add image tags to current or marked files"))
    (define-key map
      [menu-bar operate image-dired-dired-comment-files]
      '(menu-item "Add Image Comment..." image-dired-dired-comment-files
                  :help "Add image comment to current or marked files"))
    (define-key map
      [menu-bar operate image-dired-display-thumbs]
      '(menu-item "Display Image Thumbnails" image-dired-display-thumbs
                  :help "Display image thumbnails for current or marked image files"))

    (define-key map [menu-bar operate dashes-4]
      '("--"))

    (define-key map
      [menu-bar operate epa-dired-do-decrypt]
      '(menu-item "Decrypt..." epa-dired-do-decrypt
		  :help "Decrypt current or marked files"))

    (define-key map
      [menu-bar operate epa-dired-do-verify]
      '(menu-item "Verify" epa-dired-do-verify
		  :help "Verify digital signature of current or marked files"))

    (define-key map
      [menu-bar operate epa-dired-do-sign]
      '(menu-item "Sign..." epa-dired-do-sign
		  :help "Create digital signature of current or marked files"))

    (define-key map
      [menu-bar operate epa-dired-do-encrypt]
      '(menu-item "Encrypt..." epa-dired-do-encrypt
		  :help "Encrypt current or marked files"))

    (define-key map [menu-bar operate dashes-3]
      '("--"))

    (define-key map [menu-bar operate query-replace]
      '(menu-item "Query Replace in Files..." dired-do-find-regexp-and-replace
		  :help "Replace regexp matches in marked files"))
    (define-key map [menu-bar operate search]
      '(menu-item "Search Files..." dired-do-find-regexp
		  :help "Search marked files for matches of regexp"))
    (define-key map [menu-bar operate isearch-regexp]
      '(menu-item "Isearch Regexp Files..." dired-do-isearch-regexp
		  :help "Incrementally search marked files for regexp"))
    (define-key map [menu-bar operate isearch]
      '(menu-item "Isearch Files..." dired-do-isearch
		  :help "Incrementally search marked files for string"))
    (define-key map [menu-bar operate chown]
      '(menu-item "Change Owner..." dired-do-chown
		  :visible (not (memq system-type '(ms-dos windows-nt)))
		  :help "Change the owner of marked files"))
    (define-key map [menu-bar operate chgrp]
      '(menu-item "Change Group..." dired-do-chgrp
		  :visible (not (memq system-type '(ms-dos windows-nt)))
		  :help "Change the group of marked files"))
    (define-key map [menu-bar operate chmod]
      '(menu-item "Change Mode..." dired-do-chmod
		  :help "Change mode (attributes) of marked files"))
    (define-key map [menu-bar operate touch]
      '(menu-item "Change Timestamp..." dired-do-touch
		  :help "Change timestamp of marked files"))
    (define-key map [menu-bar operate load]
      '(menu-item "Load" dired-do-load
		  :help "Load marked Emacs Lisp files"))
    (define-key map [menu-bar operate compile]
      '(menu-item "Byte-compile" dired-do-byte-compile
		  :help "Byte-compile marked Emacs Lisp files"))
    (define-key map [menu-bar operate compress]
      '(menu-item "Compress" dired-do-compress
		  :help "Compress/uncompress marked files"))
    (define-key map [menu-bar operate print]
      '(menu-item "Print..." dired-do-print
		  :help "Ask for print command and print marked files"))
    (define-key map [menu-bar operate hardlink]
      '(menu-item "Hardlink to..." dired-do-hardlink
		  :help "Make hard links for current or marked files"))
    (define-key map [menu-bar operate symlink]
      '(menu-item "Symlink to..." dired-do-symlink
		  :visible (fboundp 'make-symbolic-link)
		  :help "Make symbolic links for current or marked files"))
    (define-key map [menu-bar operate async-command]
      '(menu-item "Asynchronous Shell Command..." dired-do-async-shell-command
		  :help "Run a shell command asynchronously on current or marked files"))
    (define-key map [menu-bar operate command]
      '(menu-item "Shell Command..." dired-do-shell-command
		  :help "Run a shell command on current or marked files"))
    (define-key map [menu-bar operate delete]
      '(menu-item "Delete" dired-do-delete
		  :help "Delete current file or all marked files"))
    (define-key map [menu-bar operate rename]
      '(menu-item "Rename to..." dired-do-rename
		  :help "Rename current file or move marked files"))
    (define-key map [menu-bar operate copy]
      '(menu-item "Copy to..." dired-do-copy
		  :help "Copy current file or all marked files"))

    map)
  
  "Local keymap for Dired mode buffers.")
```

## 

## 

## 

## 

## 

## 

## 

## 

## 

## 

## 

## 

## 

## 

## 

## 

## 

## 

