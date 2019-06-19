# sboosali's `~/.emacs.d`

## Features

## Installation

POSIX Installation:

```sh
cd ~ && git clone https://github.com/sboosali/.emacs.d && cd .emacs.d 

git submodule update --init --recursive

./install.bash
```

Windows Installation:

```sh
cd %USERPROFILE% && git clone https://github.com/sboosali/.emacs.d && cd .emacs.d 

git submodule update --init --recursive

install.bat
```

## Files

`install.*` — the installation scripts, which manage:

*  Submodules — install vendored libraries, under `./lib/*/`, via `git submodule update` (`git` works on most systems).
* Nix  — install required programs, under `./nix/**.nix`, viia `nix-env -i` (`nix` works only on POSIX(?) systems).

## Dependencies

### Elisp

Features (vendored):

* `use-package`-
* `helm`-
* `dante`-

Libraries (vendored):

* `dash`-2.14.1
* `s`-1.12.0
* `f`-v0.20.0

### Fonts

* `Iosevka` — 

## Related

- `magnars`'s  — <https://github.com/magnars/.emacs.d#readme>
- `jwiegley`'s — <https://github.com/jwiegley/dot-emacs#readme>

