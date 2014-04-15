#!/bin/sh
ocamlfind ocamlc -I ~/.opam/system/lib/batteries -c geom.ml || exit 1
ocamlfind ocamlc -I ~/.opam/system/lib/batteries -c voro.ml || exit 1
ocamlfind ocamlc -I ~/.opam/system/lib/cairo -I ~/.opam/system/lib/batteries \
  -c draw.ml || exit 1
ocamlfind ocamlc -I ~/.opam/system/lib/batteries -c example.ml || exit 1
ocamlfind ocamlc -g -linkpkg \
  -package batteries -package cairo -package yojson \
  geom.cmo voro.cmo draw.ml example.cmo \
  -o test \
  || exit 1
echo Build successful.
