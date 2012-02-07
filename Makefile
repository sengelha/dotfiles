SRC=$(shell find . -type f | grep -v Makefile | grep -v .git/ )
TARGETS=$(patsubst ./%,$(HOME)/.%,$(SRC))

.PHONY: all
all: $(TARGETS)

$(HOME)/.%: ./%
	@if [ -f $< ]; then \
		[ ! -d `dirname $@` ] && mkdir -p `dirname $@`; \
		echo Copying $< to $@; \
		cp $< $@; \
	fi
