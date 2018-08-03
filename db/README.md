= `db`

"DataBase" for `Emacs` packages.

== `.git-track-empty-directory`

We want to track the directory structure,
without tracking any the volatile / only-locally-relevant files within.

Thus, the `db` subdirs have an empty/stub file.

The filename is arbitrary.

== `.gitignore`

The "emacs-storage" files, e.g.:

* `savehist.el`
* `.emacs.desktop`
* <etc>

are `gitignore`d

`custom.el` is not, it's tracked.

==

