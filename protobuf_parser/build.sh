
source ../ocaml_env.sh

rm -rf *.cmx
rm -rf parser.mli
rm -rf parser.ml
rm lexer.ml
PBC_INC=../../install/build/lib/ocaml/site-lib/ocaml-protobuf/

$OCAMLYACC parser.mly
$OCAMLLEX  lexer.mll
$OCAMLOPT -c ast.ml
$OCAMLOPT -c astc.ml
$OCAMLOPT -c ast_util.mli
$OCAMLOPT -c ast_util.ml
$OCAMLOPT -c astc_util.mli
$OCAMLOPT -c astc_util.ml
$OCAMLOPT -c parser.mli
$OCAMLOPT -c lexer.ml
$OCAMLOPT -c parser.ml
$OCAMLOPT -I $PBC_INC -c backend_ocaml.ml 
$OCAMLOPT -I $PBC_INC -c main.ml 
$OCAMLOPT -I $PBC_INC -o protobuf_parser.tsk \
    ast.cmx ast_util.cmx astc.cmx astc_util.cmx parser.cmx lexer.cmx protobuf_codec.cmxa backend_ocaml.cmx main.cmx
