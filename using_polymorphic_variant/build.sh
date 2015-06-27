source ../ocaml_env.sh

# clean up
#
rm -f *.o
rm -f *.cm*
rm -f *.tsk


# export $PATH
$OCAMLFIND ocamlopt -i  main.ml
$OCAMLFIND ocamlopt -o  pmv.tsk main.ml
