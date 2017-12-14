.PHONY: all clean

OCB = ocamlbuild -use-ocamlfind

ifeq ($(shell echo $$TERM), dumb)
 OCB = $(OCB) -tag debug -classic-display
endif


all:
	$(OCB) irc.cma irc.cmxa irc.cmxs

doc:
	$(OCB) irc.docdir/index.html

clean:
	$(OCB) -clean
