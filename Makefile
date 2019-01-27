rm *.cmi
rm *.o
rm *.cmx
ocamlbuild -use-ocamlfind -cflag -g -lflag -g main.byte -pkgs str
