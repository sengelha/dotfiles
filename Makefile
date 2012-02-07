SRC=$(shell find . -type f -o -type l | grep -v Makefile | grep -v .git/ )
TARGETS=$(patsubst ./%,$(HOME)/.%,$(SRC))

.PHONY: all
all: $(TARGETS)

$(HOME)/.%: ./%
	@if [ ! -d `dirname $@` ]; then \
	    echo Creating `dirname $@`; \
	    mkdir -p `dirname $@`; \
	fi
	cp $< $@
