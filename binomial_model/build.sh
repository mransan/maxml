source ../ocaml_env.sh

# clean up
#
rm -f *.o
rm -f *.cm*
rm -f *.tsk

#$OCAMLFIND ocamlopt -package unix -linkpkg  -o algorithms.tsk sort.ml util.ml main.ml

$OCAMLOPT -o binomial_model.tsk main.ml
