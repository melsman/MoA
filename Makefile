.PHONY: all
all: lib/github.com/diku-dk/sml-setmap
	$(MAKE) -C lib/github.com/melsman/MoA all

.PHONY: test
test: all
	$(MAKE) -C lib/github.com/melsman/MoA test

.PHONY: clean
clean:
	find . -name 'MLB' | xargs rm -rf
	find . -name '*~' | xargs rm -f
	$(MAKE) -C lib/github.com/melsman/MoA clean

lib/github.com/diku-dk/sml-setmap:
	smlpkg sync
