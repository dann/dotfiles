# unbundle.vim

Unbundle activates [Vim scripts] from isolated directories by adding them to
Vim's *runtimepath* and building missing *helptags* for documentation therein.
For the initiated, it's like [pathogen.vim] but with *ftbundles* and less code.

## Terminology

**bundles** are [Vim scripts] that are stored in isolated directories under
`bundle/` subdirectories of directories specified in Vim's *runtimepath*.  For
example, `~/.vim/bundle/*/` would be your *bundles* if you were using Unix.

**ftbundles** are *filetype*-specific *bundles* that are loaded lazily (only
when they are necessary) to shorten Vim's startup time.  For example,
`~/.vim/ftbundle/{filetype}/*/` would be your *ftbundles* for `{filetype}` if
you were using Unix.

## Installation

1. Clone this Git repository or [download its contents][downloads] into a new
   `bundle/vim-unbundle` subdirectory inside your Vim runtime directory.  For
   example, `~/.vim/bundle/vim-unbundle` would be the corect location in Unix.

        cd ~/.vim/bundle/
        git clone git://github.com/sunaku/vim-unbundle.git

2. Run the following command inside Vim to start using Unbundle immediately,
   or add it to your *vimrc* file to start Unbundle whenever you start Vim.

        :runtime bundle/vim-unbundle/unbundle.vim

3. Run the following command inside Vim to learn more about using Unbundle.

        :help unbundle.vim

## Credits

* [Colin Shea](https://github.com/evaryont) came up with [the idea of
  *ftbundles*](https://github.com/sunaku/vim-unbundle/issues/2).

* [heavenshell](https://github.com/heavenshell) added [compatibility](
  https://github.com/sunaku/vim-unbundle/pull/7) with the Japanese
  [Vim-Kaoriya](http://www.kaoriya.net/software/vim) distribution.

* [Peter Aronoff](http://ithaca.arpinum.org) gave feedback and ideas on how to
  best organize filetypes with dependent ftbundles, such as eRuby templates.

* An [anonymous Alexander suggested](
  http://snk.tuxfamily.org/log/vim-script-management-system.html#IDComment98711660)
  appending `/.` to directory globs for portability across operating systems.

## License

Copyright 2010 Suraj N. Kurapati <sunaku@gmail.com>

Distributed under [the same terms as Vim itself][license].

[Vim scripts]: http://www.vim.org/scripts/
[license]: http://vimdoc.sourceforge.net/htmldoc/uganda.html#license
[downloads]: https://github.com/sunaku/vim-unbundle/downloads
[pathogen.vim]: https://github.com/tpope/vim-pathogen#readme
