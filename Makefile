SRC=$(shell find . \( -type f -o -type l \) -not -path "./.git/*" -a -not -path "./Makefile")
TARGETS=$(patsubst ./%,$(HOME)/.%,$(SRC))

.PHONY: all
all: $(TARGETS)

$(HOME)/.%: ./%
	@if [ ! -d `dirname $@` ]; then \
	    echo Creating `dirname $@`; \
	    mkdir -p `dirname $@`; \
	fi
	cp $< $@
