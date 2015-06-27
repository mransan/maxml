source ../ocaml_env.sh



$OCAMLOPT -c a.mli
$OCAMLOPT -c b.mli 
$OCAMLOPT -c a.ml
$OCAMLOPT -c b.ml
$OCAMLOPT -c main.ml


$OCAMLOPT -o module_dep.tsk a.cmx b.cmx main.cmx


