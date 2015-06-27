
source ../../ocaml_env.sh

rm -rf *.cmx

$OCAMLOPT -for-pack Sub -c a.ml
$OCAMLOPT -for-pack Sub -c b.ml
$OCAMLOPT -c sub.ml
$OCAMLOPT -pack -o sub.cmx a.cmx b.cmx 
$OCAMLOPT -a -o sub.cmxa sub.cmx 

bin=../sub_bin

rm -rf $bin/*

cp sub.cmxa $bin
cp sub.cmi  $bin 
cp a.cmi  $bin 
cp b.cmi  $bin 
cp sub.a    $bin 
$OCAMLOPT -i sub.ml > $bin/sub.mli 
