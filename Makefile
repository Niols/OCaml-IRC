.PHONY: all src examples clean

all: src examples

src:
	$(MAKE) -C src

examples:
	$(MAKE) -C examples

clean:
	$(MAKE) -C src clean
	$(MAKE) -C examples clean
	find -iname '*~' -delete
