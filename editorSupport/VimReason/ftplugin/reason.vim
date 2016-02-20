" Language:     Reason
" Description:  Vim ftplugin file for Reason

if exists("b:did_ftplugin")
	finish
endif
let b:did_ftplugin = 1

if exists('g:merlin')
  " Activate merlin on current buffer: For some reason this has to be at the top
  " of the file - is something else causing early return?
  " Ensure that merlin is activated first
  " Read the comment in syntax_checkers for a summary of the state of the world
  " of merlin integration.
  function! MerlinBinary()
    try | return executable(merlin#FindBinary()) | catch | return 0 | endtry
  endfunction

  if MerlinBinary()
    call merlin#Register()
  endif
endif


let s:save_cpo = &cpo
set cpo&vim

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set noet sw=4 ts=4:
