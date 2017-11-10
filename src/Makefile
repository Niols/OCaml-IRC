.PHONY: all clean

OCB = ocamlbuild -use-ocamlfind

ifeq ($(shell echo $$TERM), dumb)
 OCB = $(OCB) -tag debug -classic-display
endif


all:
	$(OCB) IRC.cma IRC.cmxa IRC.cmxs

clean:
	$(OCB) -clean

