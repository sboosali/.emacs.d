# Emacs No-Window

i.e. `emacs -nw ...`

## Shortcuts

the original Emacs keybindings, like `C-w` / `C-y` / `C-_` / `M-/` (i.e. CUA-mode"), were designed to not conflict with (i.e. be shadowed by) the behavior of the control key in a terminal emulator. With `C-c` being `SIGINT` (interrupt signal), `C-z` being `SIGTSTP` (stop signal), `C-v` being "insert literally".

we preserve these, as well as adding more ergonomic keybindings for the commands I use the most, like autocompletion (`dabbrev-expand`) and `undo`.

## TODO

- `<tab>`

