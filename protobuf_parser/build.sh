
source ../ocaml_env.sh

rm -rf *.cmx
rm -rf parser.mli
rm -rf parser.ml
rm lexer.ml

$OCAMLYACC parser.mly
$OCAMLLEX  lexer.mll
$OCAMLOPT -c ast.ml
$OCAMLOPT -c parser.mli
$OCAMLOPT -c lexer.ml
$OCAMLOPT -c parser.ml
$OCAMLOPT -c main.ml
$OCAMLOPT -o protobuf_parser.tsk ast.cmx parser.cmx lexer.cmx main.cmx
