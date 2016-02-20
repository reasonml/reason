VimReason: Vim support for Reason
=========================================

Installation:
============

[VimBox](https://github.com/jordwalke/vimbox) is a great way to get started with a modern Vim configuration. Eventually, `VimReason` will come pre-loaded in `VimBox`.

1. Install `merlin` using `OPAM` (`brew install opam` if needed).


    opam install merlin


2. Once you installed `VimBox` *just* install `VimReason` by adding the following to `~/.vim/bundlesVimRc` (if using `VimBox`), or wherever you specify your Vim packages if you're using something else.

```vim
" Bundle for VimReason
NeoBundle 'jordwalke/VimReason'
```
Depending on permissions, the Bundle might not get installed. To install it manually: 
```vim
    ln -s `pwd`/VimReason ~/.vim/bundle/
```

Syntastic Integration:
==========

If you installed `VimBox`, this is already set up for you. If you didn't
install `VimBox`, install `Syntastic` the same way you installed `VimReason` -
by adding the following to your `.vimrc`:

```vim
NeoBundle "https://github.com/scrooloose/syntastic"
```

Now, when you save any `.re`/`.rei` file, syntax errors and compile errors will
be shown in the location list window and the lines with errors will be
underlined in red.

Formatting:
===========

The command `:ReasonPrettyPrint` invokes the binary `reasonfmt` which must be
available on your `PATH`.

`VimReason`, doesn't use Vim's standard external formatting program bridge
because it disturbs your undo history and cursor position. Instead,
`VimReason`, implements its own buffer updates so that no modifications to your
buffer occur if the file is already formatted, and only lines requiring updates
are actually modified (so that undo/redo take you to the position where
formatting actually effected the file).

You can set `g:vimreason_extra_args_expr_reason` to control the arguments
passed to `reasonfmt` (such as `-print-width`). The contents of
`g:vimreason_extra_args_expr_reason` is a string that contains a `VimScript`
expression. This allows you do dynamically determine the formatting arguments
based on things like your window width.

  " Always wrap at 90 columns
  let g:vimreason_extra_args_expr_reason = '"-print-width 90"'

  " Wrap at the window width
  let g:vimreason_extra_args_expr_reason = '"-print-width " . ' .  "winwidth('.')"

  " Wrap at the window width but not if it exceeds 120 characters.
  let g:vimreason_extra_args_expr_reason = '"-print-width " . ' .  "min([120, winwidth('.')])"


Key Mappings:
=============

You can create a custom function and map it to a keybinding (in your `.vimrc`)
to quickly trigger formatting, and control how the formatting occurs. To enable
keymappings *only* for `reason` files, your vim must have been compiled with
`+localmap` (`:echo has('localmap')` should output `1` if your vim supports it).

For example, the following maps `cmd + shift + m` to reformat only when editing
a `reason` file.

  autocmd FileType reason map <buffer> <D-M> :ReasonPrettyPrint<Cr>


Merlin:
===========
If you have `merlin` installed, `VimReason` will also activate it for `reason`
files. Completions should work well, but most other things don't. `VimReason`
can't rely on `merlin` compilation, but does scan the `.merlin` file to pick
out flags and include paths so that it can compile individual files and report
compiler errors to syntastic.


Brace Completion:
============
`VimBox` already comes with `PairTools`, but if you don't have `VimBox`, install it using `Vundle`:

```vim
NeoBundle "git://github.com/MartinLafreniere/vim-PairTools.git"
```

Seriously - you should install `PairTools`. It is perhaps the best brace completion plugin for Vim.

Once you have `PairTools`, add this configuration in your `.vimrc`:

```vim
    autocmd FileType reason let g:pairtools_reason_pairclamp = 1
    autocmd FileType reason let g:pairtools_reason_tagwrench = 0
    autocmd FileType reason let g:pairtools_reason_jigsaw    = 1
    autocmd FileType reason let g:pairtools_reason_autoclose  = 1
    autocmd FileType reason let g:pairtools_reason_forcepairs = 0
    autocmd FileType reason let g:pairtools_reason_closepairs = "(:),[:],{:}" . ',":"'
    autocmd FileType reason let g:pairtools_reason_smartclose = 1
    autocmd FileType reason let g:pairtools_reason_smartcloserules = '\w,(,&,\*'
    autocmd FileType reason let g:pairtools_reason_antimagic  = 1
    autocmd FileType reason let g:pairtools_reason_antimagicfield  = "Comment,String,Special"
    autocmd FileType reason let g:pairtools_reason_pcexpander = 1
    autocmd FileType reason let g:pairtools_reason_pceraser   = 1
    autocmd FileType reason let g:pairtools_reason_tagwrenchhook = 'tagwrench#BuiltinNoHook'
    autocmd FileType reason let g:pairtools_reason_twexpander = 0
    autocmd FileType reason let g:pairtools_reason_tweraser   = 0
    autocmd FileType reason let g:pairtools_reason_apostrophe = 0
```

LICENSE
-------
Some files from VimReason are based on the Rust vim plugin and so we are including that license.
