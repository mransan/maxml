
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
$OCAMLOPT -w +A -c logger.mli
$OCAMLOPT -w +A -c logger.ml
$OCAMLOPT -w +A -c ast.ml
$OCAMLOPT -w +A -c astc.ml
$OCAMLOPT -w +A -c exception.mli
$OCAMLOPT -w +A-4 -c exception.ml
$OCAMLOPT -w +A -c ast_util.mli
$OCAMLOPT -w +A -c ast_util.ml
$OCAMLOPT -w +A -c astc_util.mli
$OCAMLOPT -w +A-4 -c astc_util.ml
$OCAMLOPT -w +A -c parser.mli
$OCAMLOPT -w +A -c lexer.ml
$OCAMLOPT -w +A -c parser.ml
$OCAMLOPT -w +A -I $PBC_INC -c encoding_util.ml 
$OCAMLOPT -w +A -I $PBC_INC -c backend_ocaml_static.ml 
$OCAMLOPT -w +A-4 -I $PBC_INC -c backend_ocaml.mli 
$OCAMLOPT -w +A-4 -I $PBC_INC -c backend_ocaml.ml 
$OCAMLOPT -w +A-4 -I $PBC_INC -c main.ml 
$OCAMLOPT -w -A-8-9 -I $PBC_INC -c test.ml 
$OCAMLOPT  -I $PBC_INC -o test.tsk $CMXS test.cmx
$OCAMLOPT -w +A-4 -I $PBC_INC -o ml-protoc $CMXS main.cmx
