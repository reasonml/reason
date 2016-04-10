" Vim syntastic plugin
" Language:     Reason
" Maintainer:   Jordan Walke <jordojw@gmail.com>
"Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
"
" See for details on how to add an external Syntastic checker:
" https://github.com/scrooloose/syntastic/wiki/Syntax-Checker-Guide#external
" Based on the OCaml syntastic plugin.
" Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved

function! SyntaxCheckers_reason_merlin_IsAvailable()
  if !exists("*merlin#SelectBinary")
    return 0
  endif
  let l:path = ""
  try
    if !exists("b:merlin_binary")
      let l:path = merlin#SelectBinary()
    else
      let l:path = b:merlin_binary
    endif
  catch
    return 0
  endtry
  if exists("b:merlin_path")
    let l:path = b:merlin_path
  endif
  return executable(l:path)
endfunction

function! SyntaxCheckers_reason_merlin_GetLocList()
  return merlin#ErrorLocList()
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'reason',
    \ 'name': 'merlin'})

