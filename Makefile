rm -f *.cmi *.o *.cmx *.naitve *.byte
ocamlbuild -use-ocamlfind -cflag -g -lflag -g main.byte -pkgs str
mv main.byte emeraldvm
