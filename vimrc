" vimrc

" This should come first, as it can change other settings
set nocompatible

set   autoindent
set   autowrite
set   background=dark
set   backspace=2
set nobackup
set   browsedir=buffer
set noconfirm
set noedcompatible
set   equalalways
set   errorbells
set   esckeys
set noexpandtab
set nohlsearch
set   ignorecase " required with smartcase!
set   incsearch
set   laststatus=2
set   magic
set   matchpairs=(:),{:},[:]
set   modeline
set   modelines=1
set   ruler
set   shiftwidth=8
set   showcmd
set   showfulltag
set   showmatch
set   showmode
set   smartcase
set nosmartindent " si is meant for c-like languages
set   suffixes=.aux,.bak,.dvi,.log,.ps,.swp,~,.o,.obj,.info,.swp
set   tabstop=8
set   title
set   undolevels=1000
set   updatecount=200
set   updatetime=4000
set   verbose=0
set   viminfo=""
set novisualbell
set   warn
set   wildchar=<TAB>
set   wildmenu
set   wrapscan
set nowritebackup

if has("syntax") && (has("gui_running") || &t_Co > 2)
    syntax on
endif

" Keymaps
nmap :W   :w
nmap :Q   :q
nmap <CR> o

imap <C-p> <ESC>gqipA
nmap <C-p> gqip

" Force runtimepath to include .vim in Windows
set runtimepath=~/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,~/.vim/after

filetype on
filetype plugin on
