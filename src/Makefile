.PHONY: all clean

OCB = ocamlbuild -use-ocamlfind

ifeq ($(shell echo $$TERM), dumb)
 OCB = $(OCB) -tag debug -classic-display
endif


all:
	$(OCB) irc.cma irc.cmxa irc.cmxs

clean:
	$(OCB) -clean

