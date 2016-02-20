" Vim syntastic plugin
" Language:     Reason
" Maintainer:   Jordan Walke <jordojw@gmail.com>
"Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
"
" See for details on how to add an external Syntastic checker:
" https://github.com/scrooloose/syntastic/wiki/Syntax-Checker-Guide#external
" Based on the OCaml syntastic plugin.
" Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved
if exists("g:loaded_syntastic_reason_reasonc_checker")
    finish
endif
let g:loaded_syntastic_reason_reasonc_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_reason_reasonc_IsAvailable() dict " {{{1
    return executable("reasonfmt")
endfunction " }}}1

function! syntax_checkers#reason#reasonc#Reason_postprocess(errors) abort " {{{2
    " Common build tools prepend prefixes such as m_PackageName__ModuleName to
    " globally namespace them.
    "
    " If module aliases provided a way to avoid this and implement
    " namespacing, we won't need this.
    let s:hidePrefixes = 0
    if !exists("g:syntastic_reason_hide_common_build_prefixes") || g:syntastic_reason_hide_common_build_prefixes
        let s:hidePrefixes = 1
    endif

    " echoerr string(a:errors)
    for e in a:errors
        " Reduce newlines
        let e['text'] = substitute(e['text'], "\001", '', 'g')
        let e['text'] = substitute(e['text'], '\n', ' ', 'g')
        let e['text'] = substitute(e['text'], '\m\s\{2,}', ' ', 'g')
        let e['text'] = substitute(e['text'], '\m^\s\+', '', '')
        let e['text'] = substitute(e['text'], '\m\s\+$', '', '')
        if s:hidePrefixes
          let e['text'] = substitute(e['text'], "M_\\([a-z]*\\)__", "\\1\\.", "g")
        endif
        " Adjust off by one error
        let e['col'] = e['col'] + 1
    endfor
    return a:errors
endfunction " }}}2


function! SyntaxCheckers_reason_reasonc_GetLocList() dict " {{{1
    let makeprg = s:GetMakeprg()
    if makeprg == ''
        return []
    endif

    let errorformat =
        \ '%WWarning: File "%f"\, line %l\, chars %c-%n:,'.
        \ '%WWarning: line %l\, chars %c-%n:,'.
        \ '%AFile "%f"\, line %l\, characters %c-%n:,'.
        \ '%AFile "%f"\, line %l\, characters %c-%*\d (end at line %*\d\, character %*\d):,'.
        \ '%AFile "%f"\, line %l\, character %c:,'.
        \ '%AFile "%f"\, line %l\, character %c:%m,'.
        \ '%-GPreprocessing error %.%#,'.
        \ '%-GPreprocessing error %.%#,'.
        \ '%-GCommand exited %.%#,'.
        \ '%C%tarning %*\d: %m,'.
        \ '%C%m,'.
        \ '%-G+%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'Postprocess': ['syntax_checkers#reason#reasonc#Reason_postprocess'],
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr("")} })

    for e in loclist
        if get(e, 'col', 0) && get(e, 'nr', 0)
            let e['hl'] = '\%>' . (e['col'] - 1) . 'c\%<' . (e['nr'] + 1) . 'c'
            let e['nr'] = 0
        endif
    endfor

    return loclist
endfunction " }}}1

" Utilities {{{1

" Enable this to debug
" let g:syntastic_debug=1


function! s:GetMakeprg() " {{{2
    return s:GetReasoncMakeprg()
endfunction " }}}2

function! s:GetBuildParams()
python <<EOF
buildPaths = merlin.command("path", "list", "build")
normalizedBuildPaths = []
for buildPath in buildPaths:
    normalizedBuildPaths = normalizedBuildPaths + [str(buildPath)]
vim.command("let buildPaths = %s"% str(normalizedBuildPaths))
      
EOF


    let includes = []
    for path in buildPaths
        let includes = includes + ["-I " . path]
    endfor

    if exists("b:dotmerlin") && len(b:dotmerlin) > 1
      echoerr "Multiple .merlin files active. How can this happen? " + (join (b:dotmerlin, ","))
    else
        if exists("b:dotmerlin") && len(b:dotmerlin) == 1
            let dotMerlinFile = b:dotmerlin[0]
            let dotMerlinContent = readfile(dotMerlinFile)
            let flags = []
            " Just until this is fixed
            " https://github.com/the-lambda-church/merlin/issues/375
            for line in dotMerlinContent
                if line[0:2] == "FLG"
                    let restOfLine = line[3:]
                    let splitRestOfLine = split(restOfLine)
                    for flg in splitRestOfLine
                        let flags = (flags + [flg])
                    endfor
                endif
            endfor
            return join(includes, ' ') . ' ' . join(flags, ' ') . ' '
        else
            if exists("b:dotmerlin") && len(b:dotmerlin) == 0
                " It's actually not an error. It might just not be loaded.
                " echoerr "No .merlin loaded"
                return ' '
            endif
        endif
    endif
endfunction

function! s:GetReasoncMakeprg() " {{{2
  let fileExt = expand('%:e')
  let buildParams = s:GetBuildParams()
  if fileExt == 'rei'
    " First run the preprocessor by itself, so that we catch merely the parse
    " errors. Otherwise, the preprocessor fails when given to OCaml which is
    " handled, but there's a bunch of other spew that's tough to filter out.
    let reasonPPCall = "reasonfmt -is-interface-pp -print none" . syntastic#util#shexpand('%')
    return reasonPPCall . " && ocamlc -c -pp reasonfmt " . buildParams . " -intf " . syntastic#util#shexpand('%')
  else
    " First run the preprocessor by itself, so that we catch merely the parse
    " errors. Otherwise, the preprocessor fails when given to OCaml which is
    " handled, but there's a bunch of other spew that's tough to filter out.
    let reasonPPCall = "reasonfmt -print none " . syntastic#util#shexpand('%')
    let ret = reasonPPCall . " && ocamlc -c -pp reasonfmt " . buildParams . " -impl " . syntastic#util#shexpand('%')
    return ret
  endif
endfunction " }}}2


call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'reason',
    \ 'name': 'reasonc'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:

