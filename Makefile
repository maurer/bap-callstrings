.PHONY: all clean rebuild
all: print_acyc_table.plugin print_lcsg.plugin print_tree.plugin table2tree.plugin dump_k_table.plugin print_k_table.plugin test.native
rebuild:
clean:
	rm -rf *.plugin _build

%.plugin: %.ml rebuild
	bapbuild -pkg ocamlgraph $@

test.native: rebuild
	bapbuild test.native

test: all
	./test.native
