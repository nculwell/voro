#!/bin/sh
ocamlfind ocamlc -package batteries -c geom.ml || exit 1
ocamlfind ocamlc -package batteries -c voro.ml || exit 1
ocamlfind ocamlc -package batteries,cairo2 -c draw.ml || exit 1
ocamlfind ocamlc -package batteries -c example.ml || exit 1
ocamlfind ocamlc -g -linkpkg \
  -package batteries,cairo2,yojson \
  geom.cmo voro.cmo draw.cmo example.cmo \
  -o test \
  || exit 1
echo Build successful.
