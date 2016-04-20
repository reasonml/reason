" Vim syntastic plugin helper
" Language:     Reason
" Maintainer:   Jordan Walke <jordojw@gmail.com>
" Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

if exists("g:loaded_syntastic_reason_filetype")
  finish
endif


let g:loaded_syntastic_reason_filetype = 1
let s:save_cpo = &cpo
set cpo&vim

" Tell Syntastic about Reason filetype/enables tab completion 'SyntasticInfo'
" command. Doesn't actually register the checker.
if exists('g:syntastic_extra_filetypes')
  call add(g:syntastic_extra_filetypes, 'reason')
else
  let g:syntastic_extra_filetypes = ['reason']
endif

" From auto-format plugin:
" https://github.com/Chiel92/vim-autoformat/blob/master/plugin/autoformat.vim
let g:vimreason_reason = "refmt"
let g:vimreason_args_expr_reason = '"-use-stdin true -print re " .' .  "expand('%')"
"
function! Strip(input_string)
  return substitute(a:input_string, '\s*$', '\1', '')
endfunction

function! s:set_formatprg(...)
    let type = a:0 ? a:1 : &filetype
    "Support composite filetypes by replacing dots with underscores
    let type = substitute(type, "[.]", "_", "g")

    "Get formatprg config
    let s:vimreason_var = "g:vimreason_".type
    let s:vimreason_args_var = "g:vimreason_args_".type
    let s:vimreason_args_expr_var = "g:vimreason_args_expr_".type
    let s:vimreason_extra_args_expr_var = "g:vimreason_extra_args_expr_".type

    if !exists(s:vimreason_var)
        "No formatprg defined
        if exists("g:autoformat_verbosemode")
            echoerr "No formatter defined for filetype '".type."'."
        endif
        return 0
    endif
    let s:formatprg = eval(s:vimreason_var)

    let s:vimreason_args = ""
    if exists(s:vimreason_args_expr_var)
        let s:vimreason_args = eval(eval(s:vimreason_args_expr_var))
    elseif exists(s:vimreason_args_var)
        let s:vimreason_args = eval(s:vimreason_args_var)
    endif
    let s:vimreason_extra_args = ""
    if exists(s:vimreason_extra_args_expr_var)
        let s:vimreason_extra_args = eval(eval(s:vimreason_extra_args_expr_var))
    endif

    "Set correct formatprg path, if it is installed
    if !executable(s:formatprg)
        "Configured formatprg not installed
        if exists("g:autoformat_verbosemode")
            echoerr "Defined formatter ".eval(s:vimreason_var)." is not executable."
        endif
        return 0
    endif
    let totalCommand = s:formatprg." ".s:vimreason_extra_args." ".s:vimreason_args
    " We don't even *need* to use the formatting program. In fact, it makes it
    " worse, because it replace the entire file at once which makes undo/redo
    " difficult to see changes.
    " let &formatprg = totalCommand

    " Check v:shell_error after trying to format
    " Ensure it can even be formatted
    let inLines = getline(1,'$')
    let buffContents = join(inLines, "\n")
    let out = system(totalCommand, buffContents)
    if v:shell_error
      " TODO: Pass this output to syntastic directly.
      " For now, just message it.
      let out = substitute(out, "\001", '', 'g')
      let out = substitute(out, '\n', ' ', 'g')
      let out = substitute(out, '\m\s\{2,}', ' ', 'g')
      let out = substitute(out, '\m^\s\+', '', '')
      let out = substitute(out, '\m\s\+$', '', '')
      echomsg out
      return 0
    else
      let numModifications = 0
      let outLines = split(out, '\n')
      let i = 0
      while i < len(outLines)
        if i < len(inLines)
          let outLine = Strip(outLines[i])
          let inLine = inLines[i]
          if outLine != inLine
            let numModifications = numModifications + 1
            call setline(i + 1, outLine)
          endif
        else
          let outLine = Strip(outLines[i])
          " Notice no + 1
          call append(i, outLine)
          let numModifications = numModifications + 1
        endif
        let i = i + 1
      endwhile
      let stopDeletingAt = i
      let i = len(inLines) - 1
      while i >= stopDeletingAt
        execute ((i + 1) . " delete")
        let numModifications = numModifications + 1
        let i = i - 1
      endwhile
      if numModifications == 0
        echomsg "Refmt: Already Formatted"
      endif
      return 1
    endif
  endfunction

function! DoReasonPrettyPrint()
    "Save window state
    " let winview=winsaveview()
    " Other mechanism for saving state
    " Alternative: Find the closest word under cursor, and find that it is the
    " nth occurence. After formatting, if the total number of occurences
    " haven't changed, jump to the nth occurence.
    let _s=@/
    let l = line(".")
    let c = col(".")

    " let oldFormatprg = &formatprg
    if call('<SID>set_formatprg', a:000)
        "Autoformat code
        if exists ('g:SyntasticChecker')
          execute 'SyntasticReset'
          " Can't do this till you save!
          " execute 'SyntasticCheck reasonc'
        endif
    endif
    " Need to restore this, otherwise formatting specific regions with `gq`
    " will send it to our formatter!
    " let &formatprg = oldFormatprg

    "Recall window state
    " call winrestview(winview)
    " Clean up: restore previous search history, and cursor position
    " This doesn't make redo put the cursor at the best place. To do that,
    " you'd need to diff the before and after and only delete/replace those
    " lines that have changed as a result of the formatting.
    " Play around with
    " call setline(line('.'), getline('.') . ' ' . result)
    " Where you only pass the *first* line/column that actually differed
    let @/=_s
    call cursor(l, c)
endfunction


command -nargs=* ReasonPrettyPrint :call DoReasonPrettyPrint(<f-args>)

let &cpo = s:save_cpo
unlet s:save_cpo
