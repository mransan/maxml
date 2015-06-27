
source ../../ocaml_env.sh

rm -rf *.cmx

$OCAMLOPT -for-pack Other_sub -c b.ml
$OCAMLOPT -for-pack Other_sub -c a.ml
$OCAMLOPT -pack -o other_sub.cmx a.cmx b.cmx 
$OCAMLOPT -o other_sub.cmxa -a other_sub.cmx

bin=../other_sub_bin

rm -rf $bin/*

cp other_sub.cmxa $bin
cp other_sub.cmi  $bin 
cp other_sub.a    $bin 
