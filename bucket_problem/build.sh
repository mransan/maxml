source ../ocaml_env.sh

# clean up
#
rm -f *.o
rm -f *.cm*
rm -f *.tsk

# compile ml bindings
#
FILES="
v1
v2
main
"
CMXS=""

for file in $FILES
do
  $OCAMLFIND ocamlopt -c $file.ml
  CMXS="$CMXS $file.cmx"
done

#$OCAMLOPT --help 

$OCAMLOPT -o bucket_problem.tsk $CMXS


