.PHONY: all clean
all: print_lcsg.plugin
clean:
	rm -rf *.plugin _build

%.plugin: %.ml
	bapbuild -pkg ocamlgraph $@

test.native:
	ocamlbuild -pkg ounit test.native
