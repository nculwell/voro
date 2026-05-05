#!/bin/sh
# vim: et ts=8 sts=2 sw=2
opam --version >/dev/null || {
  echo "OPAM not found. You can get it here:" >/dev/stderr
  echo " https://opam.ocaml.org/doc/Install.html" >/dev/stderr
  exit
}
eval $(opam env)
opam install batteries cairo2 yojson -y
