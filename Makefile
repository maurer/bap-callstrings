.PHONY: all clean
all: print_acyc_table.plugin print_lcsg.plugin print_tree.plugin table2tree.plugin dump_k_table.plugin print_k_table.plugin test.native
clean:
	rm -rf *.plugin _build

%.plugin: %.ml
	bapbuild -pkg ocamlgraph $@

test.native:
	bapbuild test.native
