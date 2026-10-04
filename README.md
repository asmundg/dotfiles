# dotfiles

The machine configuration installs Emacs, fonts, and external commands. This
repository contains the Emacs configuration itself.

Edit `emacs/emacs.org`, then tangle it to update `emacs/.emacs.el`:

```sh
emacs --batch --quick emacs/emacs.org --eval '(org-babel-tangle)'
```

Link the configuration into a new home directory with:

```sh
ln -s /path/to/dotfiles/emacs/.emacs.el ~/.emacs.el
ln -s /path/to/dotfiles/emacs/.emacs.d ~/.emacs.d
```

The commands fail when either destination already exists. Move the existing
file or directory aside after inspecting it, then run the command again.
