
source ../ocaml_env.sh

rm -rf *.cmx
rm -rf parser.mli
rm -rf parser.ml
rm lexer.ml
PBC_INC=../../install/build/lib/ocaml/site-lib/ocaml-protobuf/
OCAML_INC=../../install/build/lib/ocaml/site-lib/ocaml-protobuf/

CMXS="
str.cmxa 
logger.cmx
ast.cmx 
ast_util.cmx 
astc.cmx 
exception.cmx
astc_util.cmx 
parser.cmx 
lexer.cmx 
protobuf_codec.cmxa 
encoding_util.cmx
backend_ocaml_static.cmx
backend_ocaml.cmx
"

$OCAMLYACC parser.mly
$OCAMLLEX  lexer.mll
$OCAMLOPT -c logger.mli
$OCAMLOPT -c logger.ml
$OCAMLOPT -c ast.ml
$OCAMLOPT -c astc.ml
$OCAMLOPT -c exception.mli
$OCAMLOPT -c exception.ml
$OCAMLOPT -c ast_util.mli
$OCAMLOPT -c ast_util.ml
$OCAMLOPT -c astc_util.mli
$OCAMLOPT -c astc_util.ml
$OCAMLOPT -c parser.mli
$OCAMLOPT -c lexer.ml
$OCAMLOPT -c parser.ml
$OCAMLOPT -I $PBC_INC -c encoding_util.ml 
$OCAMLOPT -I $PBC_INC -c backend_ocaml_static.ml 
$OCAMLOPT -I $PBC_INC -c backend_ocaml.ml 
$OCAMLOPT -I $PBC_INC -c main.ml 
$OCAMLOPT -I $PBC_INC -c test.ml 
$OCAMLOPT -I $PBC_INC -o test.tsk $CMXS test.cmx
$OCAMLOPT -I $PBC_INC -o ml-protoc $CMXS main.cmx
