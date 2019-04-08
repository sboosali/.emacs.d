# TODO

## cabal.el

## eshell


https://www.emacswiki.org/emacs/EshellAlias

https://www.reddit.com/r/emacs/comments/b767sh/wow_eshell_is_fast/

    (defalias 'open 'find-file)
    (defalias 'openo 'find-file-other-window)


Alias 'll' to 'ls -l'


Make sure that the positional parameters are included:

    ~ $ alias ll 'ls -l $*'
Alias 'emacs' to 'find-file'
    ~ $ alias emacs 'find-file $1'
Note that without the quotes, the positional parameter will get lost:

    ~ $ alias emacs find-file $1
    ~ $ alias
    alias emacs find-file
Note also that $* will not work because ‘find-file’ expects exactly one parameter. Using $* instead of $1 will pass a list of parameters to find-file, and find-file will barf:

    ~ $ alias emacs 'find-file $*'
    ~ $ emacs test.txt
    Wrong type argument: stringp, ("test.txt")



I use a line in .bashrc which, after all aliases have been defined, creates the alias file for eshell (eshell-aliases-file):

    alias | sed -E "s/^alias ([^=]+)='(.*)'$/alias \1 \2 \$*/g; s/'\\\''/'/g;" >~/.emacs.d/eshell/alias 


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

## 

