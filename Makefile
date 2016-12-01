cflags=\
	-I `ocamlfind query lwt` \
	-I `ocamlfind query ezjsonm` \
	-I `ocamlfind query cstruct` \
	-I `ocamlfind query cohttp` \
	-I `ocamlfind query uri` \
	-I `ocamlfind query nocrypto` \
	-I `ocamlfind query hex`

.PHONY: lib
lib:
	ocamlbuild -cflags "$(cflags)" b2.cma b2.cmxa

install:
	$(MAKE) uninstall || :
	cd _build && ocamlfind install b2 ../META b2.cmxa b2.cma b2.a *.cmi *.cmx b2.mli

uninstall:
	ocamlfind remove b2

