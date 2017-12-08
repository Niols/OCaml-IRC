.PHONY: all src examples clean

src:
	$(MAKE) -C src
	mkdir -p lib && cp src/_build/irc.* lib

examples:
	$(MAKE) -C examples

clean:
	$(MAKE) -C src clean
	$(MAKE) -C examples clean
	rm -rf lib
	find -iname '*~' -delete
