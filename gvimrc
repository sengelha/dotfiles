" gvimrc

set   lines=40
set   mousehide

colors peaksea

if has("mac")
  set guifont=Monaco:h10
elseif has("gui_win32")
  set guifont=Bitstream_Vera_Sans_Mono:h8
else
"  set guifont=6x13
endif

" Remove 't' from guioptions: no tearoff menu entries
let &guioptions = substitute(&guioptions, "t", "", "g")
