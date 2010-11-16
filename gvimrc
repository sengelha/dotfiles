" gvimrc

set   lines=40
set   mousehide

colorscheme desert

if has("mac")
  set guifont=Monaco:h9
elseif has("gui_win32")
  set guifont=Bitstream_Vera_Sans_Mono:h8
else
"  set guifont=6x13
endif

" Remove 't' from guioptions: no tearoff menu entries
let &guioptions = substitute(&guioptions, "t", "", "g")
