
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
graph.cmx
pbpt.cmx 
pbpt_util.cmx 
pbtt.cmx 
exception.cmx
pbtt_util.cmx 
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
$OCAMLOPT -w +A -c graph.ml
$OCAMLOPT -w +A -c pbpt.ml
$OCAMLOPT -w +A -c pbtt.ml
$OCAMLOPT -w +A -c exception.mli
$OCAMLOPT -w +A-4 -c exception.ml
$OCAMLOPT -w +A -c pbpt_util.mli
$OCAMLOPT -w +A -c pbpt_util.ml
$OCAMLOPT -w +A -c   pbtt_util.mli
$OCAMLOPT -w +A-4 -c pbtt_util.ml
$OCAMLOPT -w +A -c parser.mli
$OCAMLOPT -w +A -c lexer.ml
$OCAMLOPT -w +A -c parser.ml
$OCAMLOPT -w +A -I $PBC_INC -c encoding_util.mli 
$OCAMLOPT -w +A -I $PBC_INC -c encoding_util.ml 
$OCAMLOPT -w +A -I $PBC_INC -c backend_ocaml_static.ml 
$OCAMLOPT -w +A-4 -I $PBC_INC -c backend_ocaml.mli 
$OCAMLOPT -w +A-4 -I $PBC_INC -c backend_ocaml.ml 
$OCAMLOPT -w +A-4 -I $PBC_INC -c main.ml 
$OCAMLOPT -w -A-8-9 -I $PBC_INC -c test.ml 
$OCAMLOPT  -I $PBC_INC -o test.tsk $CMXS test.cmx
$OCAMLOPT -w +A-4 -I $PBC_INC -o ml-protoc $CMXS main.cmx
