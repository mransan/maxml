source ../ocaml_env.sh

# clean up
#
rm -f *.o
rm -f *.cm*
rm -f *.tsk


# export $PATH
$OCAMLFIND ocamlopt -i  dimension.ml
$OCAMLFIND ocamlopt -thread -c  thread_example.ml
$OCAMLFIND ocamlopt -o  dimension.tsk dimension.ml
