" File: mru.vim
" Author: Yegappan Lakshmanan (yegappan AT yahoo DOT com)
" Version: 3.0
" Last Modified: January 12, 2008
"
" Overview
" --------
" The Most Recently Used (MRU) plugin provides an easy access to a list of
" recently opened/edited files in Vim. This plugin automatically stores the
" file names as you open/edit them in Vim.
"
" This plugin will work on all the platforms where Vim is supported. This
" plugin will work in both console and GUI Vim. This version of the MRU
" plugin needs Vim 7.0 and above. If you are using an earlier version of
" Vim, then you should use an older version of the MRU plugin.
"
" The recently used filenames are stored in a file specified by the Vim
" MRU_File variable.
"
" Installation
" ------------
" 1. Copy the mru.vim script to the $HOME/.vim/plugin or the
"    $HOME/vimfiles/plugin or the $VIM/vimfiles directory.  Refer to the
"    ':help add-plugin', ':help add-global-plugin' and ':help runtimepath'
"    topics for more details about Vim plugins.
" 2. Set the MRU_File Vim variable in the .vimrc file to the location of a
"    file to store the most recently edited file names. This step is needed
"    only if you want to change the default MRU filename.
" 3. Restart Vim.
" 4. You can use the ":MRU" command to list and edit the recently used files.
"    In GUI Vim, you can use the 'File->Recent Files' menu to access the
"    recently used files.
"
" Usage
" -----
" You can use the ":MRU" command to list all the most recently edited file
" names. The file names will be listed in a temporary Vim window. If the MRU
" window is already opened, then the MRU list displayed in the window will be
" refreshed.
"
" If you are using GUI Vim, then the names of the recently edited files are
" added to the "File->Recent Files" menu. You can select the name of a file
" from this sub-menu to edit the file.
"
" You can use the normal Vim commands to move around in the MRU window. You
" cannot make changes in the MRU window.
"
" You can select a file name to edit by pressing the <Enter> key or by double
" clicking the left mouse button on a file name.  The selected file will be
" opened. If the file is already opened in a window, the cursor will be moved
" to that window. Otherwise, the file is opened in the previous window. If the
" previous window has a modified buffer or is the preview window or is used by
" some other plugin, then the file is opened in a new window.
"
" You can press the 'o' key to open the file name under the cursor in the
" MRU window in a new window.
"
" To open a file from the MRU window in a new tab, press the 't' key.  If the
" file is already opened in a window in the current or in another tab, then
" the cursor is moved to that tab. Othewrise, a new tab is opened.
"
" You can press the 'u' key in the MRU window to update the file list. This is
" useful if you keep the MRU window open always.
"
" You can close the MRU window by pressing the 'q' key or using one of the Vim
" window commands.
"
" To display only files matching a pattern from the MRU list in the MRU
" window, you can specify a pattern to the ":MRU" command. For example, to
" display only file names containing "vim" in them, you can use the following
" command ":MRU vim"
"
" You can use the ":MRUedit" command to edit files from the MRU list. This
" command supports completion of file names from the MRU list. You can specify
" a file either by the name or by the location in the MRU list.
"
" Whenever the MRU list changes, the MRU file is updated with the latest MRU
" list. When you have multiple instances of Vim running at the same time, the
" latest MRU list will show up in all the instances of Vim.
"
" Configuration
" -------------
" By changing the following variables you can configure the behavior of this
" plugin. Set the following variables in your .vimrc file using the 'let'
" command.
"
" The list of recently edit file names is stored in the file specified by the
" MRU_File variable.  The default setting for this variable is
" $HOME/.vim_mru_files for Unix systems and $VIM/_vim_mru_files for non-Unix
" systems. You can change this variable to point to a file by adding the
" following line to the .vimrc file:
"
"       let MRU_File = 'd:\myhome\_vim_mru_files'
"
" By default, the plugin will remember the names of the last 10 used files.
" As you edit more files, old file names will be removed from the MRU list.
" You can set the 'MRU_Max_Entries' variable to remember more file names. For
" example, to remember 20 most recently used file names, you can use
"
"       let MRU_Max_Entries = 20
"
" By default, all the edited file names will be added to the MRU list. If you
" want to exclude file names matching a list of patterns, you can set the
" MRU_Exclude_Files variable to a list of Vim regular expressions. By default,
" this variable is set to an empty string. For example, to not include files
" in the temporary (/tmp, /var/tmp and d:\temp) directories, you can set the
" MRU_Exclude_Files variable to
"
"       let MRU_Exclude_Files = '^/tmp/.*\|^/var/tmp/.*'  " For Unix
"       let MRU_Exclude_Files = '^c:\\temp\\.*'           " For MS-Windows
" 
" The specified pattern should be a Vim regular expression pattern.
"
" The default height of the MRU window is 8. You can set the MRU_Window_Height
" variable to change the window height.
"
"       let MRU_Window_Height = 15
"
" By default, when the :MRU command is invoked, the MRU list will be displayed
" in a new window. Instead, if you want the MRU plugin to reuse the current
" window, then you can set the 'MRU_Use_Current_Window' variable to one.
"
"       let MRU_Use_Current_Window = 1
"
" The MRU plugin will reuse the current window. When a file name is selected,
" the file is also opened in the current window.
"
" When you select a file from the MRU window, the MRU window will be
" automatically closed and the selected file will be opened in the previous
" window. You can set the 'MRU_Auto_Close' variable to zero to keep the MRU
" window open.
"
"       let MRU_Auto_Close = 0
"
" ****************** Do not modify after this line ************************
if exists('loaded_mru_autoload')
    finish
endif
let loaded_mru_autoload=1

if v:version < 700
    finish
endif

" Line continuation used here
let s:cpo_save = &cpo
set cpo&vim

" Maximum number of entries allowed in the MRU list
if !exists('MRU_Max_Entries')
    let MRU_Max_Entries = 10
endif

" Files to exclude from the MRU list
if !exists('MRU_Exclude_Files')
    let MRU_Exclude_Files = ''
endif

" Height of the MRU window
" Default height is 8
if !exists('MRU_Window_Height')
    let MRU_Window_Height = 8
endif

if !exists('MRU_Use_Current_Window')
    let MRU_Use_Current_Window = 0
endif

if !exists('MRU_Auto_Close')
    let MRU_Auto_Close = 1
endif

if !exists('MRU_File')
    if has('unix')
        let MRU_File = $HOME . "/.vim_mru_files"
    else
        let MRU_File = $VIM . "/_vim_mru_files"
    endif
endif

" MRU_LoadList
" Load the latest MRU file list from the MRU file
function! s:MRU_LoadList()
    " Read the list from the MRU file.
    if filereadable(g:MRU_File)
        let s:MRU_files = readfile(g:MRU_File)
        if s:MRU_files[0] =~# '^" Most recently edited files in Vim'
            " Generated by the previous version of the MRU plugin. Ignore the
            " list
            let s:MRU_files = []
        elseif s:MRU_files[0] =~# '^#'
            " Remove the comment line
            call remove(s:MRU_files, 0)
        endif
    else
        let s:MRU_files = []
    endif

    " Refresh the MRU menu
    call s:MRU_Refresh_Menu()
endfunction

" MRU_SaveList
" Save the MRU list to the file
function! s:MRU_SaveList()
    let l = []
    call add(l, '# Most recently edited files in Vim (version 3.0)')
    call extend(l, s:MRU_files)
    call writefile(l, g:MRU_File)
endfunction

" MRU_AddFile
" Add a file to the MRU file list
function! s:MRU_AddFile(acmd_bufnr)
    " Get the full path to the filename
    let fname = fnamemodify(bufname(a:acmd_bufnr + 0), ':p')
    if fname == ''
        return
    endif

    " Skip temporary buffer with buftype set
    if &buftype != ''
        return
    endif

    if g:MRU_Exclude_Files != ''
        " Do not add files matching the pattern specified in the
        " MRU_Exclude_Files to the MRU list
        if fname =~? g:MRU_Exclude_Files
            return
        endif
    endif

    " If the filename is already present in the MRU list, then move
    " it to the beginning of the list
    let idx = index(s:MRU_files, fname)
    if idx == -1
        if !filereadable(fname)
            " File is not readable and is not in the MRU list
            return
        endif
    endif

    " Load the latest MRU file list
    call s:MRU_LoadList()

    " Remove the new file name from the existing MRU list (if already present)
    call filter(s:MRU_files, 'v:val !=# fname')

    " Add the new file list to the beginning of the updated old file list
    call insert(s:MRU_files, fname, 0)

    " Return the trimmed list
    if len(s:MRU_files) > g:MRU_Max_Entries
        call remove(s:MRU_files, g:MRU_Max_Entries, -1)
    endif

    " Save the updated MRU list
    call s:MRU_SaveList()

    " Refresh the MRU menu
    call s:MRU_Refresh_Menu()

    " If the MRU window is open, update the displayed MRU list
    let bname = '__MRU_Files__'
    let winnum = bufwinnr(bname)
    if winnum != -1
        let cur_winnr = winnr()
        call s:MRU_Open_Window()
        if winnr() != cur_winnr
            exe cur_winnr . 'wincmd w'
        endif
    endif
endfunction

" MRU_Edit_File
" Edit the specified file
function! s:MRU_Edit_File(filename)
    let fname = escape(a:filename, ' %#')
    " If the file is already open in one of the windows, jump to it
    let winnum = bufwinnr('^' . fname . '$')
    if winnum != -1
        if winnum != winnr()
            exe winnum . 'wincmd w'
        endif
    else
        if &modified || &buftype != '' || &previewwindow
            " Current buffer has unsaved changes or is a special buffer or is
            " the preview window.  So open the file in a new window
            exe 'split ' . fname
        else
            exe 'edit ' . fname
        endif
    endif
endfunction

" MRU_Window_Edit_File
" Open a file selected from the MRU window
"   win_opt == useopen, open file in previous window
"   win_opt == newwin, open file in new window
"   win_opt == newtab, open file in new tab
function! s:MRU_Window_Edit_File(win_opt)
    let fname = getline('.')

    if fname == ''
        return
    endif

    let fname = escape(fname, ' %#')

    if a:win_opt == 'newwin'
        " Edit the file in a new window
        exe 'leftabove new ' . fname

        if g:MRU_Auto_Close == 1 && g:MRU_Use_Current_Window == 0
            " Go back to the MRU window and close it
            let cur_winnr = winnr()

            wincmd p
            silent! close

            if winnr() != cur_winnr
                exe cur_winnr . 'wincmd w'
            endif
        endif
    elseif a:win_opt == 'newtab'
        if g:MRU_Auto_Close == 1 && g:MRU_Use_Current_Window == 0
            " Automatically close the window if the file window is
            " not used to display the MRU list.
            silent! close
        endif

        " If the selected file is already open in the current tab or in
        " another tab, jump to it. Otherwise open it in a new tab
        if bufwinnr('^' . fname . '$') == -1
            let tabnum = -1
            let i = 1
            let bnum = bufnr('^' . fname . '$')
            while i <= tabpagenr('$')
                if index(tabpagebuflist(i), bnum) != -1
                    let tabnum = i
                    break
                endif
                let i += 1
            endwhile

            if tabnum != -1
                " Goto the tab containing the file
                exe 'tabnext ' . i
            else
                " Open a new tab as the last tab page
                exe '999tabnew ' . escape(fname, ' ')
            endif
        endif

        " Jump to the window containing the file
        let winnum = bufwinnr('^' . fname . '$')
        if winnum != winnr()
            exe winnum . 'wincmd w'
        endif
    else
        " If the selected file is already open in one of the windows,
        " jump to it
        let winnum = bufwinnr('^' . fname . '$')
        if winnum != -1
            if g:MRU_Auto_Close == 1 && g:MRU_Use_Current_Window == 0
                " Automatically close the window if the file window is
                " not used to display the MRU list.
                silent! close
            endif
            " As the window numbers will change after closing a window,
            " get the window number again and jump to it, if the cursor
            " is not already in that window
            let winnum = bufwinnr('^' . fname . '$')
            if winnum != winnr()
                exe winnum . 'wincmd w'
            endif
        else
            if g:MRU_Auto_Close == 1 && g:MRU_Use_Current_Window == 0
                " Automatically close the window if the file window is
                " not used to display the MRU list.
                silent! close

                " Jump to the window from which the MRU window was opened
                if exists('s:MRU_last_buffer')
                    let last_winnr = bufwinnr(s:MRU_last_buffer)
                    if last_winnr != -1 && last_winnr != winnr()
                        exe last_winnr . 'wincmd w'
                    endif
                endif
            else
                if g:MRU_Use_Current_Window == 0
                    " Goto the previous window
                    " If MRU_Use_Current_Window is set to one, then the
                    " current window is used to open the file
                    wincmd p
                endif
            endif

            " Edit the file
            if &modified || &buftype != '' || &previewwindow
                " Current buffer has unsaved changes or is a special buffer or
                " is the preview window.  So open the file in a new window
                exe 'split ' . fname
            else
                exe 'edit ' . fname
            endif
        endif
    endif
endfunction

" MRU_Open_Window
" Display the Most Recently Used file list in a temporary window.
function! s:MRU_Open_Window(...)

    " Load the latest MRU file list
    call s:MRU_LoadList()

    " Empty MRU list
    if empty(s:MRU_files)
        echohl WarningMsg | echo 'MRU file list is empty' | echohl None
        return
    endif

    " Save the current buffer number. This is used later to open a file when a
    " entry is selected from the MRU window. The window number is not saved,
    " as the window number will change when new windows are opened.
    let s:MRU_last_buffer = bufnr('%')

    let bname = '__MRU_Files__'

    " If the window is already open, jump to it
    let winnum = bufwinnr(bname)
    if winnum != -1
        if winnr() != winnum
            " If not already in the window, jump to it
            exe winnum . 'wincmd w'
        endif

        setlocal modifiable

        " Delete the contents of the buffer to the black-hole register
        silent! %delete _
    else
        if g:MRU_Use_Current_Window
            " Reuse the current window
            "
            " If the __MRU_Files__ buffer exists, then reuse it. Otherwise open
            " a new buffer
            let bufnum = bufnr(bname)
            if bufnum == -1
                let cmd = 'edit ' . bname
            else
                let cmd = 'buffer ' . bufnum
            endif

            exe cmd

            if bufnr('%') != bufnr(bname)
                " Failed to edit the MRU buffer
                return
            endif
        else
            " Open a new window at the bottom

            " If the __MRU_Files__ buffer exists, then reuse it. Otherwise open
            " a new buffer
            let bufnum = bufnr(bname)
            if bufnum == -1
                let wcmd = bname
            else
                let wcmd = '+buffer' . bufnum
            endif

            exe 'silent! botright ' . g:MRU_Window_Height . 'split ' . wcmd
        endif
    endif

    " Mark the buffer as scratch
    setlocal buftype=nofile
    setlocal bufhidden=delete
    setlocal noswapfile
    setlocal nowrap
    setlocal nobuflisted
    " Use fixed height for the MRU window
    if v:version >= 602
        setlocal winfixheight
    endif

    " Setup the cpoptions properly for the maps to work
    let old_cpoptions = &cpoptions
    set cpoptions&vim

    " Create a mapping to jump to the file
    nnoremap <buffer> <silent> <CR>
                \ :call <SID>MRU_Window_Edit_File('useopen')<CR>
    nnoremap <buffer> <silent> o
                \ :call <SID>MRU_Window_Edit_File('newwin')<CR>
    nnoremap <buffer> <silent> t
                \ :call <SID>MRU_Window_Edit_File('newtab')<CR>
    nnoremap <buffer> <silent> u :MRU<CR>
    nnoremap <buffer> <silent> <2-LeftMouse>
                \ :call <SID>MRU_Window_Edit_File('useopen')<CR>
    nnoremap <buffer> <silent> q :close<CR>

    " Restore the previous cpoptions settings
    let &cpoptions = old_cpoptions

    " Display the MRU list
    if a:0 == 0
        " No search pattern specified. Display the complete list
        silent! 0put =s:MRU_files
    else
        " Display only the entries matching the specified pattern
        silent! 0put =filter(copy(s:MRU_files), 'v:val =~? a:1')
    endif

    " Move the cursor to the beginning of the file
    exe 1

    setlocal nomodifiable
endfunction

" MRU_Edit_Complete
" Command-line completion function used by :MRUedit command
function! s:MRU_Edit_Complete(ArgLead, CmdLine, CursorPos)
    if a:ArgLead == ''
        " Return the list of MRU files
        return s:MRU_files
    else
        return filter(copy(s:MRU_files), 'v:val =~? a:ArgLead')
    endif
endfunction

" MRU_Edit_File_Cmd
" Function to handle the MRUedit command
function! s:MRU_Edit_File_Cmd(fspec)
    if a:fspec == ''
        return
    endif

    " Load the latest MRU file
    call s:MRU_LoadList()

    " User can specify either a number or a partial file name to
    " edit a file from the MRU list
    if a:fspec =~ '^\d\+$'
        " A number is specified
        let fnum = a:fspec
        if fnum <= 0 || fnum > len(s:MRU_files)
            echohl WarningMsg
            echo 'Error: valid range of values is 1 - ' . len(s:MRU_files)
            echohl None
            return
        endif

        " User index is 1 based, but the internal index is 0 based
        let fnum -= 1

        let fname = s:MRU_files[fnum]
        if fname != ''
            call s:MRU_Edit_File(fname)
        endif
    else
        " Locate the file name matching the supplied partial name

        " Escape backslash in the partial filename, otherwise regexp
        " comparison will fail
        let fpat = escape(a:fspec, '\')

        let l = filter(copy(s:MRU_files), 'v:val =~ fpat')
        if len(l) == 0
            echohl WarningMsg
            echo 'Error: No matching file for ' . a:fspec
            echohl None
            return
        endif

        if len(l) > 1
            echohl WarningMsg
            echo 'Error: More than one match for ' . a:fspec
            echohl None
            return
        endif

        let fname = l[0]

        call s:MRU_Edit_File(fname)
    endif
endfunction

" MRU_Refresh_Menu()
" Refresh the MRU menu
function! s:MRU_Refresh_Menu()
    if !has('menu')
        " No support for menus
        return
    endif

    " Setup the cpoptions properly for the maps to work
    let old_cpoptions = &cpoptions
    set cpoptions&vim

    " Remove the MRU menu
    " To retain the teared-off MRU menu, we need to add a dummy entry
    silent! unmenu &File.Recent\ Files
    noremenu &File.Recent\ Files.Dummy <Nop>
    silent! unmenu! &File.Recent\ Files

    anoremenu <silent> &File.Recent\ Files.Refresh\ list
                \ :call <SID>MRU_LoadList()<CR>
    anoremenu File.Recent\ Files.-SEP1-           :

    " Add the filenames in the MRU list to the menu
    for fname in s:MRU_files
        " Escape special characters in the filename
        let esc_fname = escape(fnamemodify(fname, ':t'), ". \\|\t%#")

        " Truncate the directory name if it is long
        let dir_name = fnamemodify(fname, ':h')
        let len = strlen(dir_name)
        " Shorten long file names by adding only few characters from
        " the beginning and end.
        if len > 30
            let dir_name = strpart(dir_name, 0, 10) .
                        \ '...' . 
                        \ strpart(dir_name, len - 20)
        endif
        let esc_dir_name = escape(dir_name, ". \\|\t")

        exe 'anoremenu <silent> &File.Recent\ Files.' . esc_fname .
                    \ '\ (' . esc_dir_name . ')' .
                    \ " :call <SID>MRU_Edit_File('" . fname . "')<CR>"
    endfor

    " Remove the dummy menu entry
    unmenu &File.Recent\ Files.Dummy

    " Restore the previous cpoptions settings
    let &cpoptions = old_cpoptions
endfunction

" Load the MRU list on plugin startup
call s:MRU_LoadList()

" Autocommands to detect the most recently used files
autocmd BufRead * call s:MRU_AddFile(expand('<abuf>'))
autocmd BufNewFile * call s:MRU_AddFile(expand('<abuf>'))
autocmd BufWritePost * call s:MRU_AddFile(expand('<abuf>'))

" Command to open the MRU window
command! -nargs=? MRU call s:MRU_Open_Window(<q-args>)
command! -nargs=1 -complete=customlist,s:MRU_Edit_Complete MRUedit
            \ call s:MRU_Edit_File_Cmd(<q-args>)

" restore 'cpo'
let &cpo = s:cpo_save
unlet s:cpo_save
