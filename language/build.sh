
source ../ocaml_env.sh

rm -rf *.cmx
rm parser.mli
rm parser.ml
rm lexer.ml

$OCAMLOPT -c ast.ml
$OCAMLYACC parser.mly
$OCAMLLEX  lexer.mll
$OCAMLOPT -c parser.mli
$OCAMLOPT -c lexer.ml
$OCAMLOPT -c parser.ml
$OCAMLOPT -c main.ml
$OCAMLOPT -o ast.tsk parser.cmx lexer.cmx ast.cmx main.cmx
