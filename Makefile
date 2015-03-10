.PHONY: all clean
all: call_sensitivity_graph.plugin print_lcsg.plugin
clean:
	rm -rf *.plugin _build

%.plugin: %.cmxs
	bapbuild -pkg ocamlgraph $@

%.cmxs: %.ml
	bapbuild -pkg ocamlgraph $@

test.native:
	ocamlbuild -pkg ounit test.native
